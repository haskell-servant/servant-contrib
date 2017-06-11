{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE UndecidableInstances #-}
module Servant.Server.Contrib.ManyQueryParams (
    ManyQueryParams,
    applyDefaults,
    -- * Internals
    KSFHAD,
    ) where

import Data.Proxy (Proxy (..))
import Data.Semigroup ((<>))
import GHC.TypeLits (Symbol, KnownSymbol, symbolVal)
import Generics.SOP as SOP 
import Network.HTTP.Types (parseQueryText)
import Servant.API
import Servant.Server
import Servant.Contrib.ManyQueryParams
import Servant.Server.Internal.RoutingApplication (DelayedIO, addParameterCheck, withRequest, delayedFailFatal)

import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.ByteString.Lazy as BSL
import qualified Network.Wai as Wai

import Unsafe.Coerce (unsafeCoerce)

-- |
--
-- >>> data Quu = Quu (Maybe Text) (Maybe Int) deriving Show
-- >>> type QuuParams = '[ '("foo", Text), '("bar", Int) ]
-- >>> type MyApi = ManyQueryParams QuuParams Quu :> Get '[JSON] Int
--
-- The handler type is what we would expect:
--
-- >>> Refl :: Server MyApi :~: (Quu -> Handler Int)
-- Refl
--
-- It's possible to use @generics-sop@ to apply the default arguments.
--
instance
    ( IsProductType a (MapMaybeSnd xs)
    , All KSFHAD xs
    , HasServer api ctx
    )
    => HasServer (ManyQueryParams xs a :> api) ctx
  where
    type ServerT (ManyQueryParams xs a :> api) m = a -> ServerT api m

    route Proxy context subserver = route (Proxy :: Proxy api) context $
        subserver `addParameterCheck` withRequest paramsCheck
      where
        paramsCheck :: Wai.Request -> DelayedIO a
        paramsCheck = fmap construct . go

        construct :: NP PM xs -> a
        construct = to . SOP . Z . nppToNpi

        go :: forall (ys :: [(Symbol, *)]).
              ( SListI ys
              , All KSFHAD ys
              )
           => Wai.Request
           -> DelayedIO (NP PM ys)
        go req = hsequence' $ hcpure (Proxy :: Proxy KSFHAD) mk
          where 
            querytext = parseQueryText $ Wai.rawQueryString req

            mk :: forall p. KSFHAD p => (DelayedIO :.: PM) p
            mk = Comp $ case lookup paramname querytext of
                Nothing       -> return $ PM Nothing -- param absent from the query string
                Just Nothing  -> return $ PM Nothing -- param present with no value -> Nothing
                Just (Just v) ->
                  case parseQueryParam v of
                      Left e -> delayedFailFatal err400
                          { errBody = BSL.fromStrict $ TE.encodeUtf8 $
                              "Error parsing query parameter "
                              <> paramname <> " failed: " <> e
                          }

                      Right param -> return $ PM $ Just param
              where
                paramname = T.pack $ symbolVal (Proxy :: Proxy (Fst p))
              

-- | KnownSymbol and FromHttpApiData
class    (KnownSymbol (Fst p), FromHttpApiData (Snd p)) => KSFHAD p
instance (KnownSymbol (Fst p), FromHttpApiData (Snd p)) => KSFHAD p

-------------------------------------------------------------------------------
-- coerce
-------------------------------------------------------------------------------

-- | "proof"
_nppToNpi :: NP PM xs -> NP I (MapMaybeSnd xs)
_nppToNpi Nil = Nil
_nppToNpi (PM x :* xs) = I x :* nppToNpi xs 

-- | "fast proof"
nppToNpi :: NP PM xs -> NP I (MapMaybeSnd xs)
nppToNpi = unsafeCoerce

npiToNpp :: NP I (MapMaybeSnd xs) -> NP PM xs
npiToNpp = unsafeCoerce

-------------------------------------------------------------------------------
-- contrib
-------------------------------------------------------------------------------

newtype PM (p :: (a, *)) = PM (Maybe (Snd p))

type family Fst (p :: (a, b)) :: a where
    Fst '(a, b) = a

type family Snd (p :: (a, b)) :: b where
    Snd '(a, b) = b

type family MapMaybeSnd (ps :: [(a, b)]) :: [b] where
    MapMaybeSnd '[]       = '[]
    MapMaybeSnd (p ': ps) = Maybe (Snd p) ': MapMaybeSnd ps

-------------------------------------------------------------------------------
-- Defaults
-------------------------------------------------------------------------------

-- | Apply defaults arguments.
--
-- === Examples ===
--
-- Reusing the @Quu@ type
--
-- >>> data Quu = Quu (Maybe Text) (Maybe Int) deriving (Show, GHC.Generic)
-- >>> instance Generic Quu
-- >>> type QuuParams = '[ '("foo", Text), '("bar", Int) ]
--
-- >>> applyDefaults (Proxy :: Proxy QuuParams) (Quu (Just "def") (Just 42)) (Quu Nothing (Just 1337))
-- Quu (Just "def") (Just 1337)
--
applyDefaults
    :: forall xs a. (IsProductType a (MapMaybeSnd xs), SListI xs)
    => Proxy xs
    -> a  -- ^ defaults
    -> a  -- ^ arguments
    -> a
applyDefaults _ x y = to . SOP . Z . nppToNpi $ go
    (npiToNpp . unZ . unSOP . from $ x)
    (npiToNpp . unZ . unSOP . from $ y)
  where
    go :: NP PM xs -> NP PM xs -> NP PM xs
    go = hzipWith $ \a b -> case b of
        PM Nothing  -> a
        PM (Just _) -> b
        

-------------------------------------------------------------------------------
-- generics-sop-0.3.1.0
-------------------------------------------------------------------------------

type IsProductType (a :: *) (xs :: [*]) = (Generic a, Code a ~ '[ xs ])

-------------------------------------------------------------------------------
-- Doctest
-------------------------------------------------------------------------------

-- $setup
--
-- >>> :set -XDataKinds
-- >>> :set -XDeriveGeneric
-- >>> :set -XOverloadedStrings
-- >>> :set -XTemplateHaskell
-- >>> :set -XTypeOperators
-- >>> import Data.Text (Text)
-- >>> import Data.Type.Equality
-- >>> import qualified GHC.Generics as GHC
