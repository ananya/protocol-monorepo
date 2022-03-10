{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}

-- | Liquidity Concept
--
-- Terminology:
--  * Untyped liquidity: Liquidity with unspecified types
--  * Untapped liquidity: Type of liquidity that can be freely used by any sub-systems (without tag)
--  * Tapped liquidity: Type of liquidity that must be specific to a sub-system (with tag)
--
-- Known sub-systems to be introduced later:
--  * Agreements (TBA, CFA, IDA, etc.)
--  * Atomic Composite Agreement (ACA)
--  * Buffer Based Solvency (BBS)
--
module Money.Superfluid.Concepts.Liquidity where

import           Data.Default
import           Data.Typeable

-- | (Untyped) Liquidity Type Class
--
-- Naming conventions:
--  * Type name: lq
--  * AccountingUnit type indexer: AU_LQ
class (Default lq, Num lq, Ord lq, Show lq) => Liquidity lq

-- | Liquidity Type
--
-- Naming conventions:
--  * Term name: liqt
--
data LiquidityTag = LiquidityTag TypeRep String
liquidityTypeTag :: LiquidityTag -> String
liquidityTypeTag (LiquidityTag _ x) = x
instance Eq LiquidityTag where (==) (LiquidityTag a _) (LiquidityTag b _) = a == b
instance Show LiquidityTag where show = liquidityTypeTag

-- | TypedLiquidity Type Class
--
-- Naming conventions:
--  * Type name: tlq
--
class Liquidity lq => TypedLiquidity tlq lq | tlq -> lq where
    untypeLiquidity :: tlq -> lq

-- | UntappedLiquidity Type
--
-- Naming conventions:
--  * Term name: uliq
--
data UntappedLiquidity lq = UntappedLiquidity lq
instance Liquidity lq => TypedLiquidity (UntappedLiquidity lq) lq where
    untypeLiquidity (UntappedLiquidity liq) = liq
instance Liquidity lq => Show (UntappedLiquidity lq) where
    show (UntappedLiquidity uliq) = show uliq ++ "@_"

-- | TappedLiquidity Type
--
-- Naming conventions for TappedLiquidity:
--  * Term name: tliq
--
data TappedLiquidity lq = TappedLiquidity lq LiquidityTag
instance Liquidity lq => TypedLiquidity (TappedLiquidity lq) lq where
    untypeLiquidity (TappedLiquidity liq _) = liq
instance Liquidity lq => Show (TappedLiquidity lq) where
    show (TappedLiquidity liq liqt) = show liq ++ "@" ++ show liqt
isOfLiquidityTag :: Liquidity lq => LiquidityTag -> TappedLiquidity lq -> Bool
isOfLiquidityTag liqt1 (TappedLiquidity _ liqt2) = liqt1 == liqt2
getLiquidityOfType :: Liquidity lq => LiquidityTag -> TappedLiquidity lq -> lq
getLiquidityOfType liqt1 (TappedLiquidity liq liqt2) = if liqt1 == liqt2 then liq else def

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
    -- LiquidityVector*Timestamp multiplication function and operator aliases
    lqvXts :: lqv -> ts -> lq
    (~~*) :: lqv -> ts ->lq
    (~~*) = lqvXts
    (*~~) :: ts -> lqv ->lq
    (*~~) = flip lqvXts
