{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Superfluid.Concepts.BaseTypes where

import           Data.Default

-- | Liquidity Type Class
--
-- Naming conventions:
--  * Type name: lq
--  * Type family name: AU_LQ
class (Default lq, Num lq, Ord lq, Show lq) => Liquidity lq

type LiquidityType = Int

data (Liquidity lq) => TappedLiquidity lq = TappedLiquidity lq LiquidityType

-- | Timestamp Type Class
--
-- Naming conventions:
--  * Type name: ts
--  * Type family name: AU_TS
class (Default ts, Integral ts, Ord ts, Show ts) => Timestamp ts

-- | LiquidityVelocity Type
newtype (Liquidity lq) => LiquidityVelocity lq = LiquidityVelocity lq

lqTimesTs :: (Liquidity lq, Timestamp ts) => LiquidityVelocity lq -> ts -> lq
lqTimesTs (LiquidityVelocity liq) = (* liq) . fromInteger . toInteger
