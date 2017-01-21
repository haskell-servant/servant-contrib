{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveDataTypeable  #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | A module providing regexp validated textual data.
--
-- This module is intended to imported qualified
--
-- > import qualified Servant.Contrib.RegexHttpApiData as RE
--
-- The 'RE' type can be used, for example to do quick'n'dirty @servant@ apis.
-- In the long run it's valuable to write specific types for 'Capture's or
-- 'QueryParam's, but this type can help at the beginning.
--
-- @
-- type API = "data" :> Capture "city" (RE.RE "a-zA-Z+") :> Get '[JSON] SomeData
-- @
module Servant.Contrib.RegexHttpApiData (
    RE,
    mk,
    unsafeMk,
    original,
    groups,
    ) where

import           Data.Maybe
                 (fromJust)
import           Data.Proxy
                 (Proxy (..))
import           Data.Semigroup
                 ((<>))
import qualified Data.Text       as T
import           Data.Typeable
                 (Typeable)
import           GHC.TypeLits
                 (KnownSymbol, Symbol, symbolVal)
import qualified Text.Regex.TDFA as R
import           Web.HttpApiData
                 (FromHttpApiData (..), ToHttpApiData (..))

-- | A 'Text' value "smartly" constructed to match the given reg-exp.
--
-- It also contains a lazily construted list of capture groups.
--
-- === Examples
--
-- >>> mk "foo" :: Maybe (RE "^foo$")
-- Just (RE "foo" [["foo"]])
--
-- >>> mk "abc" :: Maybe (RE "(abc")
-- Nothing
--
-- >>> mk "abc" :: Maybe (RE "foo")
-- Nothing
data RE (re :: Symbol) = RE !T.Text [[T.Text]]
  deriving (Show, Typeable)

-- | Construct 'RE' value.
-- If regexp is invalid, or value doesn't match it 'Nothing' is returned.
mk :: forall re. KnownSymbol re => T.Text -> Maybe (RE re)
mk source = do
    regex <- R.makeRegexM regexStr :: Maybe R.Regex
    if R.match regex sourceStr
        then return $ RE source $ (fmap . fmap) T.pack $ R.match regex sourceStr
        else Nothing
  where
    regexStr = symbolVal (Proxy :: Proxy re)
    sourceStr = T.unpack source

-- | Partial 'mk'
unsafeMk :: KnownSymbol re => T.Text -> RE re
unsafeMk = fromJust . mk

-- | Extract original string.
--
-- >>> original (unsafeMk "foobar" :: RE "o")
-- "foobar"
--
-- prop> t == original (unsafeMk t :: RE ".*")
original :: RE re -> T.Text
original (RE t _) = t

-- | Extract matching groups
--
-- >>> groups (unsafeMk "foobooo" :: RE "o+")
-- [["oo"],["ooo"]]
groups :: RE re -> [[T.Text]]
groups (RE _ gs) = gs

-------------------------------------------------------------------------------
-- http-api-data
-------------------------------------------------------------------------------

instance KnownSymbol re => FromHttpApiData (RE re) where
    parseUrlPiece =
        maybe (Left $ "doesn't match regex: " <> regexStr) Right . mk
      where
        regexStr = T.pack $ symbolVal (Proxy :: Proxy re)

instance ToHttpApiData (RE re) where
    toUrlPiece = original

-------------------------------------------------------------------------------
-- Doctest setup
-------------------------------------------------------------------------------

-- $setup
-- >>> :set -XDataKinds -XOverloadedStrings
-- >>> import Test.QuickCheck (polyQuickCheck)
-- >>> import Test.QuickCheck.Instances ()
-- >>> import Language.Haskell.TH.Syntax (mkName)
