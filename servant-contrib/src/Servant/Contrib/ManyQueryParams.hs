{-# LANGUAGE KindSignatures, DataKinds #-} 
module Servant.Contrib.ManyQueryParams (
    ManyQueryParams
    -- * Internal
    ) where

import GHC.TypeLits (Symbol)

-- | 
--
-- >>> data Quu = Quu (Maybe Text) (Maybe Int)
-- >>> type MyApi = ManyQueryParams '[ '("foo", Text), ("bar", Int) ] Quu
data ManyQueryParams (xss :: [(Symbol, *)]) (a :: *)
