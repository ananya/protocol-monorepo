{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Money.Superfluid.Concepts.AccountingUnit
    ( AccountingUnit (..)
    ) where

import           Data.Kind                                 (Type)
import           Data.Typeable

import           Money.Superfluid.Concepts.Liquidity
import           Money.Superfluid.Concepts.RealtimeBalance

-- | AccountingUnit Type Class
--
-- Naming conventions:
--  * Type name : au
--
class ( Typeable au
      , Liquidity (AU_LQ au)
      , Timestamp (AU_TS au)
      , LiquidityVelocity (AU_LQV au) (AU_LQ au) (AU_TS au)
      , RealtimeBalance (AU_RTB au) (AU_LQ au)
      ) => AccountingUnit au where

    -- Associated Types
    type AU_LQ au :: Type
    type AU_TS au :: Type
    type AU_LQV au :: Type
    type AU_RTB au :: Type
