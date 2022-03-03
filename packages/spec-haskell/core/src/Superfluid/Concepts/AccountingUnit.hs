{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}

module Superfluid.Concepts.AccountingUnit
    ( AccountingUnit (..)
    ) where

import           Superfluid.Concepts.BaseTypes
import           Superfluid.Concepts.RealtimeBalance

-- | AccountingUnit Type Class
--
-- Naming conventions:
--  * Type name : au
--
class ( Liquidity (AU_LQ au)
      , Timestamp (AU_TS au)
      , RealtimeBalance (AU_RTB au) (AU_LQ au)
      ) => AccountingUnit au where
    type AU_LQ au :: *
    type AU_TS au :: *
    type AU_RTB au :: *
