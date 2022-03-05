{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Superfluid.Concepts.BaseTypes where

import           Data.Default
import           Data.Typeable

-- | Liquidity Type Class
--
-- Naming conventions:
--  * Type name: lq
--  * Type family name: AU_LQ
class (Default lq, Num lq, Ord lq, Show lq) => Liquidity lq

-- | TappedLiquidity Type and Operations
--
-- Naming conventions:
--  * Type name: tlq
--  * Term name: tliq
data Liquidity lq => TappedLiquidity lq = TappedLiquidity lq TypeRep

isOfLiquidityType :: Liquidity lq => TypeRep -> TappedLiquidity lq -> Bool
isOfLiquidityType liqt1 (TappedLiquidity _ liqt2) = liqt1 == liqt2

getLiquidityOfType :: Liquidity lq => TypeRep -> TappedLiquidity lq -> lq
getLiquidityOfType liqt1 (TappedLiquidity uliq liqt2) = if liqt1 == liqt2 then uliq else def


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
