{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Money.Superfluid.Concepts.AccountingUnit
    ( AccountingUnit (..)
    ) where

import           Data.Kind                           (Type)

import           Money.Superfluid.Concepts.BaseTypes
import           Money.Superfluid.Concepts.RealtimeBalance

-- | AccountingUnit Type Class
--
-- Naming conventions:
--  * Type name : au
--
class ( Liquidity (AU_LQ au)
      , Timestamp (AU_TS au)
      , RealtimeBalance (AU_RTB au) (AU_LQ au)
      ) => AccountingUnit au where
    type AU_LQ au :: Type
    type AU_TS au :: Type
    type AU_RTB au :: Type
