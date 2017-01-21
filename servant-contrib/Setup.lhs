\begin{code}
{-# LANGUAGE CPP #-}
#ifndef MIN_VERSION_Cabal
#define MIN_VERSION_Cabal(x,y,z) 0
#endif
#ifndef MIN_VERSION_directory
#define MIN_VERSION_directory(x,y,z) 0
#endif
#if MIN_VERSION_Cabal(1,24,0)
#define InstalledPackageId UnitId
#endif
module Main (main) where

import Control.Monad ( when )
import Data.List ( nub )
import Distribution.Package ( InstalledPackageId )
import Distribution.Package ( PackageId, Package (..), packageVersion )
import Distribution.PackageDescription ( PackageDescription(), TestSuite(..) , Library (..), BuildInfo (..))
import Distribution.Simple ( defaultMainWithHooks, UserHooks(..), simpleUserHooks )
import Distribution.Simple.Utils ( rewriteFile, createDirectoryIfMissingVerbose )
import Distribution.Simple.BuildPaths ( autogenModulesDir )
import Distribution.Simple.Setup ( BuildFlags(buildDistPref, buildVerbosity), fromFlag)
import Distribution.Simple.LocalBuildInfo ( withPackageDB, withLibLBI, withTestLBI, LocalBuildInfo(), ComponentLocalBuildInfo(componentPackageDeps), compiler )
import Distribution.Simple.Compiler ( showCompilerId , PackageDB (..))
import Distribution.Text ( display , simpleParse )
import System.FilePath ( (</>) )

#if MIN_VERSION_Cabal(1,25,0)
import Distribution.Simple.BuildPaths ( autogenComponentModulesDir )
#endif

#if MIN_VERSION_directory(1,2,2)
import System.Directory (makeAbsolute)
#else
import System.Directory (getCurrentDirectory)
import System.FilePath (isAbsolute)

makeAbsolute :: FilePath -> IO FilePath
makeAbsolute p | isAbsolute p = return p
               | otherwise    = do
    cwd <- getCurrentDirectory
    return $ cwd </> p
#endif

main :: IO ()
main = defaultMainWithHooks simpleUserHooks
  { buildHook = \pkg lbi hooks flags -> do
     generateBuildModule flags pkg lbi
     buildHook simpleUserHooks pkg lbi hooks flags
  }

generateBuildModule :: BuildFlags -> PackageDescription -> LocalBuildInfo -> IO ()
generateBuildModule flags pkg lbi = do
  let verbosity = fromFlag (buildVerbosity flags)
  let distPref = fromFlag (buildDistPref flags)

  -- Package DBs
  let dbStack = withPackageDB lbi ++ [ SpecificPackageDB $ distPref </> "package.conf.inplace" ]
  let dbFlags = "-hide-all-packages" : packageDbArgs dbStack

  withLibLBI pkg lbi $ \lib libcfg -> do
    let libBI = libBuildInfo lib

    -- modules
    let modules = exposedModules lib ++ otherModules libBI
    -- it seems that doctest is happy to take in module names, not actual files!
    let module_sources = modules

    -- We need the directory with library's cabal_macros.h!
#if MIN_VERSION_Cabal(1,25,0)
    let libAutogenDir = autogenComponentModulesDir lbi libcfg
#else
    let libAutogenDir = autogenModulesDir lbi
#endif

    -- Lib sources and includes
    iArgs <- mapM (fmap ("-i"++) . makeAbsolute) $ libAutogenDir : hsSourceDirs libBI
    includeArgs <- mapM (fmap ("-I"++) . makeAbsolute) $ includeDirs libBI

    -- CPP includes, i.e. include cabal_macros.h
    let cppFlags = map ("-optP"++) $
            [ "-include", libAutogenDir ++ "/cabal_macros.h" ]
            ++ cppOptions libBI

    -- Actually we need to check whether testName suite == "doctests"
    -- pending https://github.com/haskell/cabal/pull/4229 getting into GHC HEAD tree
    withTestLBI pkg lbi $ \suite suitecfg -> when (testName suite == "doctests") $ do

      -- get and create autogen dir
#if MIN_VERSION_Cabal(1,25,0)
      let testAutogenDir = autogenComponentModulesDir lbi suitecfg
#else
      let testAutogenDir = autogenModulesDir lbi
#endif
      createDirectoryIfMissingVerbose verbosity True testAutogenDir

      -- write autogen'd file
      rewriteFile (testAutogenDir </> "Build_doctests.hs") $ unlines
        [ "module Build_doctests where"
        , ""
        -- -package-id etc. flags
        , "pkgs :: [String]"
        , "pkgs = " ++ (show $ formatDeps $ testDeps libcfg suitecfg)
        , ""
        , "flags :: [String]"
        , "flags = " ++ show (iArgs ++ includeArgs ++ dbFlags ++ cppFlags)
        , ""
        , "module_sources :: [String]"
        , "module_sources = " ++ show (map display module_sources)
        ]
  where
    -- we do this check in Setup, as then doctests don't need to depend on Cabal
    isOldCompiler = maybe False id $ do
      a <- simpleParse $ showCompilerId $ compiler lbi
      b <- simpleParse "7.5"
      return $ packageVersion (a :: PackageId) < b

    formatDeps = map formatOne
    formatOne (installedPkgId, pkgId)
      -- The problem is how different cabal executables handle package databases
      -- when doctests depend on the library
      | packageId pkg == pkgId = "-package=" ++ display pkgId
      | otherwise              = "-package-id=" ++ display installedPkgId

    -- From Distribution.Simple.Program.GHC
    packageDbArgs :: [PackageDB] -> [String]
    packageDbArgs | isOldCompiler = packageDbArgsConf
                  | otherwise     = packageDbArgsDb

    -- GHC <7.6 uses '-package-conf' instead of '-package-db'.
    packageDbArgsConf :: [PackageDB] -> [String]
    packageDbArgsConf dbstack = case dbstack of
      (GlobalPackageDB:UserPackageDB:dbs) -> concatMap specific dbs
      (GlobalPackageDB:dbs)               -> ("-no-user-package-conf")
                                           : concatMap specific dbs
      _ -> ierror
      where
        specific (SpecificPackageDB db) = [ "-package-conf=" ++ db ]
        specific _                      = ierror
        ierror = error $ "internal error: unexpected package db stack: "
                      ++ show dbstack

    -- GHC >= 7.6 uses the '-package-db' flag. See
    -- https://ghc.haskell.org/trac/ghc/ticket/5977.
    packageDbArgsDb :: [PackageDB] -> [String]
    -- special cases to make arguments prettier in common scenarios
    packageDbArgsDb dbstack = case dbstack of
      (GlobalPackageDB:UserPackageDB:dbs)
        | all isSpecific dbs              -> concatMap single dbs
      (GlobalPackageDB:dbs)
        | all isSpecific dbs              -> "-no-user-package-db"
                                           : concatMap single dbs
      dbs                                 -> "-clear-package-db"
                                           : concatMap single dbs
     where
       single (SpecificPackageDB db) = [ "-package-db=" ++ db ]
       single GlobalPackageDB        = [ "-global-package-db" ]
       single UserPackageDB          = [ "-user-package-db" ]
       isSpecific (SpecificPackageDB _) = True
       isSpecific _                     = False

testDeps :: ComponentLocalBuildInfo -> ComponentLocalBuildInfo -> [(InstalledPackageId, PackageId)]
testDeps xs ys = nub $ componentPackageDeps xs ++ componentPackageDeps ys

\end{code}
