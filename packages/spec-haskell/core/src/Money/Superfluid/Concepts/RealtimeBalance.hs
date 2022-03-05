{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Money.Superfluid.Concepts.RealtimeBalance
    ( LiquidityVector (..)
    , mkUntypedLiquidityVector
    , untappedFromLiquidityVector
    , RealtimeBalance (..)
    , untappedLiquidityFromRTB
    , liquidityRequiredForRTB
    , RealtimeBalanceAsNum (..)
    , RealtimeBalanceAsShow (..)
    ) where

import           Data.Default

import           Money.Superfluid.Concepts.Liquidity (Liquidity, TypedLiquidity (..), getUntyppedLiquidity)

-- | LiquidityVector Sum Type and Operations
--
data Liquidity lq => LiquidityVector lq =
    UntypedLiquidityVector lq [lq] |
    TypedLiquidityVector lq [TypedLiquidity lq]

mkUntypedLiquidityVector :: Liquidity lq => [lq] -> LiquidityVector lq
mkUntypedLiquidityVector (uliq:xs) = UntypedLiquidityVector uliq xs
mkUntypedLiquidityVector _         = error "Untapped liquidity missing"

untappedFromLiquidityVector :: Liquidity lq => LiquidityVector lq -> lq
untappedFromLiquidityVector (UntypedLiquidityVector uliq _) = uliq
untappedFromLiquidityVector (TypedLiquidityVector uliq _)   = uliq

-- | RealtimeBalance Type Class
--
-- Naming conventions:
--  * Type name : rtb
--  * Type family name: AU_RTB
--  * Term name: *RTB *Balance
class (Liquidity lq, Num rtb, Default rtb) => RealtimeBalance rtb lq | rtb -> lq where
    rawLiquidityVectorFromRTB :: rtb -> [lq]
    typedLiquidityVectorFromRTB :: rtb -> LiquidityVector lq
    untappedLiquidityToRTB :: lq -> rtb
    liquidityVectorToRTB :: LiquidityVector lq -> rtb

untypedLiquidityVectorFromRTB :: (Liquidity lq, RealtimeBalance rtb lq) => rtb -> LiquidityVector lq
untypedLiquidityVectorFromRTB = mkUntypedLiquidityVector . rawLiquidityVectorFromRTB

-- | Get untapped liquidity component of the Realtme balance vector
untappedLiquidityFromRTB :: (Liquidity lq, RealtimeBalance rtb lq) => rtb -> lq
untappedLiquidityFromRTB = untappedFromLiquidityVector . untypedLiquidityVectorFromRTB

liquidityRequiredForRTB :: (Liquidity lq, RealtimeBalance rtb lq) => rtb -> lq
liquidityRequiredForRTB = foldr (+) def . rawLiquidityVectorFromRTB

-- | RealtimeBalanceAsNum DerivingVia Helper Type
--
-- To use:
--   - enable DerivingVia language extension
--   - do @deriving Num via RTB.RealtimeBalanceAsNum SimpleRealtimeBalance Wad@
--
newtype (Liquidity lq, RealtimeBalance rtb lq) => RealtimeBalanceAsNum rtb lq = RealtimeBalanceAsNum rtb
instance (Liquidity lq, RealtimeBalance rtb lq) => Num (RealtimeBalanceAsNum rtb lq) where
    (+) (RealtimeBalanceAsNum a) (RealtimeBalanceAsNum b) = RealtimeBalanceAsNum $
        liquidityVectorToRTB . mkUntypedLiquidityVector $
        zipWith (+) (rawLiquidityVectorFromRTB a) (rawLiquidityVectorFromRTB b)
    (*) = error "No definition"
    fromInteger x = RealtimeBalanceAsNum $
        untappedLiquidityToRTB . fromInteger $ x
    signum (RealtimeBalanceAsNum x) = RealtimeBalanceAsNum $
        untappedLiquidityToRTB . signum . liquidityRequiredForRTB $ x
    abs (RealtimeBalanceAsNum x) = RealtimeBalanceAsNum $
        untappedLiquidityToRTB . abs . liquidityRequiredForRTB $ x
    negate (RealtimeBalanceAsNum x) = RealtimeBalanceAsNum $
        untappedLiquidityToRTB . negate . liquidityRequiredForRTB $ x

newtype (Liquidity lq, RealtimeBalance rtb lq) => RealtimeBalanceAsShow rtb lq = RealtimeBalanceAsShow rtb
instance (Liquidity lq, RealtimeBalance rtb lq) => Show (RealtimeBalanceAsShow rtb lq) where
    show (RealtimeBalanceAsShow rtb) =
        (show . liquidityRequiredForRTB $ rtb) ++ " " ++
        (showDetail . typedLiquidityVectorFromRTB $ rtb)
        where
        showDetail (TypedLiquidityVector uliq tvec) = "( "
            ++ show uliq
            ++ foldl ((++) . (++ ", ")) "" ((map show) . (filter ((/= def) . getUntyppedLiquidity )) $ tvec)
            ++ " )"
        showDetail _                                = error "Expecting typed liquidity vector"
