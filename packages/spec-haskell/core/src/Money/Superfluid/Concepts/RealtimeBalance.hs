{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}

module Money.Superfluid.Concepts.RealtimeBalance
    ( UntypedLiquidityVector (..)
    , TypedLiquidityVector (..)
    , RealtimeBalance (..)
    , untappedLiquidityFromRTB
    , liquidityRequiredForRTB
    , RealtimeBalanceAsNum (..)
    , RealtimeBalanceAsShow (..)
    ) where

import           Data.Default

import           Money.Superfluid.Concepts.Liquidity (Liquidity, TappedLiquidity, UntappedLiquidity)

-- | UntypedLiquidityVector type
--
data Liquidity lq => UntypedLiquidityVector lq = UntypedLiquidityVector lq [lq]

_mkUntypedLiquidityVector :: Liquidity lq => [lq] -> UntypedLiquidityVector lq
_mkUntypedLiquidityVector (uliq:xs) = UntypedLiquidityVector uliq xs
_mkUntypedLiquidityVector _         = error "Untapped liquidity missing"

_getUntypedUntappedLiquidity :: Liquidity lq => UntypedLiquidityVector lq -> lq
_getUntypedUntappedLiquidity (UntypedLiquidityVector uliq _) = uliq

-- | TypedLiquidityVector type
--
data Liquidity lq => TypedLiquidityVector lq = TypedLiquidityVector (UntappedLiquidity lq) [TappedLiquidity lq]

-- | RealtimeBalance Type Class
--
-- Naming conventions:
--  * Type name : rtb
--  * Type family name: SFT_RTB
--  * Term name: *RTB *Balance
class (Liquidity lq, Num rtb, Default rtb, Show rtb) => RealtimeBalance rtb lq | rtb -> lq where
    rawLiquidityVectorFromRTB :: rtb -> [lq]
    typedLiquidityVectorFromRTB :: rtb -> TypedLiquidityVector lq
    untappedLiquidityToRTB :: lq -> rtb
    typedLiquidityVectorToRTB :: TypedLiquidityVector lq -> rtb
    untypedLiquidityVectorToRTB :: UntypedLiquidityVector lq -> rtb

-- | Get untapped liquidity component of the Realtme balance vector
untappedLiquidityFromRTB :: (Liquidity lq, RealtimeBalance rtb lq) => rtb -> lq
untappedLiquidityFromRTB = _getUntypedUntappedLiquidity . _mkUntypedLiquidityVector . rawLiquidityVectorFromRTB

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
        untypedLiquidityVectorToRTB . _mkUntypedLiquidityVector $
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
            -- ++ foldl ((++) . (++ ", ")) "" ((map show) . (filter ((/= def) . untypeLiquidity )) $ tvec)
            ++ foldl ((++) . (++ ", ")) "" (map show tvec)
            ++ " )"
