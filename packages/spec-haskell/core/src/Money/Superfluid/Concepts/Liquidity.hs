{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Money.Superfluid.Concepts.Liquidity where

import           Data.Default
import           Data.Typeable

-- | Liquidity Concept
--
-- Terminology:
--  * Untyped liquidity: Liquidity with unspecified types
--  * Untapped liquidity: Type of liquidity that can be freely used by any sub-systems
--  * Tapped liquidity: Type of liquidity that must be specific to a sub-system
--
-- Known sub-systems to be introduced later:
--  * Agreements (TBA, CFA, IDA, etc.)
--  * Atomic Composite Agreement (ACA)
--  * Buffer Based Solvency (BBS)
--

-- | (Untyped) Liquidity Type Class
--
-- Naming conventions:
--  * Type name: lq
--  * AccountingUnit type indexer: AU_LQ
--  * Term name:
--    - liq
--    - uliq (to be used as untapped liquidity)
class (Default lq, Num lq, Ord lq, Show lq) => Liquidity lq

-- | Liquidity Type
--
-- Naming conventions:
--  * Term name: liqt
--
data LiquidityType = LiquidityType TypeRep String
liquidityTypeTag :: LiquidityType -> String
liquidityTypeTag (LiquidityType _ x) = x
instance Eq LiquidityType where (==) (LiquidityType a _) (LiquidityType b _) = a == b
instance Show LiquidityType where show = liquidityTypeTag

-- | TypedLiquidity Sum Type and Operations
--
-- Naming conventions for UntappedLiquidity:
--  * Term name: uliq
--
-- Naming conventions for TappedLiquidity:
--  * Term name: tliq
data Liquidity lq => TypedLiquidity lq =
    UntappedLiquidity lq |
    TappedLiquidity lq LiquidityType

getUntyppedLiquidity :: Liquidity lq => TypedLiquidity lq -> lq
getUntyppedLiquidity (UntappedLiquidity liq) = liq
getUntyppedLiquidity (TappedLiquidity liq _) = liq

isOfLiquidityType :: Liquidity lq => LiquidityType -> TypedLiquidity lq -> Bool
isOfLiquidityType _ (UntappedLiquidity _)         = False
isOfLiquidityType liqt1 (TappedLiquidity _ liqt2) = liqt1 == liqt2

getLiquidityOfType :: Liquidity lq => LiquidityType -> TypedLiquidity lq -> lq
getLiquidityOfType _ (UntappedLiquidity _)           = def
getLiquidityOfType liqt1 (TappedLiquidity liq liqt2) = if liqt1 == liqt2 then liq else def

instance Liquidity lq => Show (TypedLiquidity lq) where
    show (TappedLiquidity liq liqt) = (show liq) ++ "@" ++ (show liqt)
    show (UntappedLiquidity uliq)   = show uliq ++ "@_"

-- | Timestamp Type Class
--
-- Naming conventions:
--  * Type name: ts
--  * AccountingUnit type indexer: AU_TS
class (Default ts, Integral ts, Ord ts, Show ts) => Timestamp ts

-- | LiquidityVelocity Type
--
-- Naming conventions:
--  * Type name: lqv
--  * AccountingUnit type indexer: AU_LQV
--  * Term name: liqv
class (Liquidity lq, Timestamp ts, Liquidity lqv) => LiquidityVelocity lqv lq ts | lqv -> lq, lqv -> ts where
    liquidityPerTimeUnit :: lq -> lqv
    liquidityTimesTimeUnit :: lqv -> lq
    lqvXts :: lqv -> ts -> lq
