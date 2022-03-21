{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Money.Superfluid.Concepts.TaggedTypeable where

import           Data.Typeable

class (Typeable a) => TaggedTypeable a where
    proxyTag :: Proxy a -> String

typeTag :: forall a. TaggedTypeable a => a -> String
typeTag _ = proxyTag (Proxy @a)
