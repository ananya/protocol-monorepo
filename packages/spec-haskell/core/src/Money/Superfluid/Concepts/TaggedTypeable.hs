module Money.Superfluid.Concepts.TaggedTypeable where

import           Data.Typeable

class (Typeable tag) => TaggedTypeable tag where
    typeTag :: Proxy tag -> String
