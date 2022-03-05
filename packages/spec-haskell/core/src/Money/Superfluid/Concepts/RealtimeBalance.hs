{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Money.Superfluid.Concepts.RealtimeBalance
    ( RealtimeBalance (..)
    , untappedLiquidityToRTB
    , untappedLiquidityFromRTB
    , liquidityRequiredForRTB
    , RealtimeBalanceAsNum (..)
    ) where

import           Data.Default

import           Money.Superfluid.Concepts.BaseTypes (Liquidity, TappedLiquidity)


-- | RealtimeBalance Type Class
--
-- Naming conventions:
--  * Type name : rtb
--  * Type family name: AU_RTB
--  * Term name: *RTB *Balance
--
-- Convention for liquidity vector:
-- * Structure: [untappedLiquidity, tappedLiquidities...]
-- * Untapped liquidity is the liquidity can be used by any sub-system
-- * Tapped liquidity must be specific to an sub-system
--
-- Known sub-systems to be introduced later:
-- * Agreements (TBA, CFA, IDA, etc.)
-- * Atomic Composite Agreement (ACA)
-- * Buffer Based Solvency (BBS)
class (Liquidity lq, Num rtb, Default rtb) => RealtimeBalance rtb lq | rtb -> lq where
    liquidityVectorFromRTB :: rtb -> [lq]
    liquidityVectorToRTB :: [lq] -> rtb
    typedLiquidityVectorToRTB :: lq -> [TappedLiquidity lq] -> rtb

untappedLiquidityToRTB :: (Liquidity lq, RealtimeBalance rtb lq) => lq -> rtb
untappedLiquidityToRTB = flip typedLiquidityVectorToRTB []

-- | Get untapped liquidity component of the Realtme balance vector
untappedLiquidityFromRTB :: (Liquidity lq, RealtimeBalance rtb lq) => rtb -> lq
untappedLiquidityFromRTB = (!! 0) . liquidityVectorFromRTB

liquidityRequiredForRTB :: (Liquidity lq, RealtimeBalance rtb lq) => rtb -> lq
liquidityRequiredForRTB = foldr (+) def . liquidityVectorFromRTB

-- | RealtimeBalanceAsNum DerivingVia Helper Type
--
-- To use:
--   - enable DerivingVia language extension
--   - do @deriving Num via RTB.RealtimeBalanceAsNum SimpleRealtimeBalance Wad@
--
newtype (RealtimeBalance rtb lq) => RealtimeBalanceAsNum rtb lq = RealtimeBalanceAsNum rtb
instance (Liquidity lq, RealtimeBalance rtb lq) => Num (RealtimeBalanceAsNum rtb lq) where
    (+) (RealtimeBalanceAsNum a) (RealtimeBalanceAsNum b) = RealtimeBalanceAsNum $
        liquidityVectorToRTB $ zipWith (+) (liquidityVectorFromRTB a) (liquidityVectorFromRTB b)
    -- (*) (RealtimeBalanceAsNum a) (RealtimeBalanceAsNum b) = RealtimeBalanceAsNum $
    --     liquidityVectorToRTB $ zipWith (*) (liquidityVectorFromRTB a) (liquidityVectorFromRTB b)
    (*) = error "No definition"
    fromInteger x = RealtimeBalanceAsNum
        $ untappedLiquidityToRTB . fromInteger $ x
    signum (RealtimeBalanceAsNum x) = RealtimeBalanceAsNum $
        untappedLiquidityToRTB . signum . liquidityRequiredForRTB $ x
    abs (RealtimeBalanceAsNum x) = RealtimeBalanceAsNum $
        untappedLiquidityToRTB . abs . liquidityRequiredForRTB $ x
    negate (RealtimeBalanceAsNum x) = RealtimeBalanceAsNum $
        untappedLiquidityToRTB . negate . liquidityRequiredForRTB $ x
