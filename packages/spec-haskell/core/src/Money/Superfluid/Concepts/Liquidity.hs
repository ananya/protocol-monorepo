{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}

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
module Money.Superfluid.Concepts.Liquidity
    -- Untyped Liquidity
    ( Liquidity
    -- Typed Liquidity
    , TypedLiquidity (..)
    -- Untapped Liquidity
    , UntappedLiquidity (..)
    , untappedLiquidityTag
    -- Tapped Liquidity
    , TappedLiquidityTag
    , AnyTappedLiquidityTag (..)
    , TappedLiquidity (..)
    -- Timestamp & LiquidityVelocity
    , Timestamp
    , LiquidityVelocity (..)
    ) where

import           Data.Default
import           Data.Typeable

import           Money.Superfluid.Concepts.TaggedTypeable

-- | (Untyped) Liquidity Type Class
--
-- Naming conventions:
--  * Type name: lq
--  * SuperfluidTypes type indexer: SFT_LQ
class (Default lq, Num lq, Ord lq, Show lq) => Liquidity lq

-- | TypedLiquidity Type Class
--
-- Naming conventions:
--  * Type name: tlq
--
class (Liquidity lq, Show tlq) => TypedLiquidity tlq lq | tlq -> lq where
    untypeLiquidity :: tlq -> lq
    isOfTypeTag :: TaggedTypeable tag => tlq -> Proxy tag -> Bool
    getLiquidityOfType :: (Liquidity lq, TypedLiquidity tlq lq, TaggedTypeable tag) => tlq -> Proxy tag -> lq
    getLiquidityOfType tliq tag = if tliq `isOfTypeTag` tag then untypeLiquidity tliq else def

-- | UntappedLiquidity Type
--
-- Naming conventions:
--  * Term name: uliq
--
newtype UntappedLiquidity lq = UntappedLiquidity lq deriving (Default)

untappedLiquidityTag :: Proxy UntappedLiquidity
untappedLiquidityTag = Proxy @UntappedLiquidity

instance Liquidity lq => TypedLiquidity (UntappedLiquidity lq) lq where
    untypeLiquidity (UntappedLiquidity liq) = liq
    isOfTypeTag _ liqt1 = typeRep liqt1 == typeRep untappedLiquidityTag

instance Liquidity lq => Show (UntappedLiquidity lq) where
    show (UntappedLiquidity liq) = show liq ++ "@_"

-- | TappedLiquidityTag Tags and its Exisistential GADTs AnyType
--
class TaggedTypeable ltag => TappedLiquidityTag ltag

data AnyTappedLiquidityTag where
    MkTappedLiquidityTag :: TappedLiquidityTag ltag => Proxy ltag -> AnyTappedLiquidityTag

-- | TappedLiquidity Type
--
-- Naming conventions for TypedLiquidity:
--  * Term name: tliq
--
newtype TappedLiquidity lq = TappedLiquidity (AnyTappedLiquidityTag, lq)

instance Liquidity lq => TypedLiquidity (TappedLiquidity lq) lq where
    untypeLiquidity (TappedLiquidity (_, liq)) = liq
    isOfTypeTag (TappedLiquidity (MkTappedLiquidityTag tag1, _)) tag2 = typeRep tag1 == typeRep tag2

instance Liquidity lq => Show (TappedLiquidity lq) where
    show (TappedLiquidity (MkTappedLiquidityTag tagProxy, liq)) = show liq ++ "@" ++ proxyTag tagProxy

-- | Timestamp Type Class
--
-- Naming conventions:
--  * Type name: ts
--  * SuperfluidTypes type indexer: SFT_TS
class (Default ts, Integral ts, Ord ts, Show ts) => Timestamp ts

-- | LiquidityVelocity Type
--
-- Naming conventions:
--  * Type name: lqv
--  * SuperfluidTypes type indexer: SFT_LQV
--  * Term name: liqv
class (Liquidity lq, Timestamp ts, Liquidity lqv, Show lqv)
    => LiquidityVelocity lqv lq ts
    | lqv -> lq, lqv -> ts where
    liquidityPerTimeUnit :: lq -> lqv
    liquidityTimesTimeUnit :: lqv -> lq
    -- LiquidityVector*Timestamp multiplication function and operator aliases
    lqvXts :: lqv -> ts -> lq
    (~~*) :: lqv -> ts ->lq
    (~~*) = lqvXts
    (*~~) :: ts -> lqv ->lq
    (*~~) = flip lqvXts
