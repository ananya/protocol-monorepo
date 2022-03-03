{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Superfluid.Concepts.RealtimeBalance
    ( RealtimeBalance (..)
    , RealtimeBalanceAsNum (..)
    ) where

import           Data.Default

import           Superfluid.Concepts.BaseTypes (Liquidity)


-- | RealtimeBalance Type Class
--
-- Naming conventions:
--  * Type name : rtb
--  * Type family name: AU_RTB
--  * Term name: *RTB *Balance
--
class (Liquidity lq, Num rtb, Default rtb) => RealtimeBalance rtb lq | rtb -> lq where
    availableBalance :: rtb -> lq
    toBalanceVector :: rtb -> [lq]
    fromBalanceVector :: [lq] -> rtb
    liquidityToRTB :: lq -> rtb
    liquidityFromRTB :: rtb -> lq
    liquidityFromRTB = foldr (+) def . toBalanceVector

-- | RealtimeBalanceAsNum DerivingVia Helper Type
--
-- To use:
--   - enable DerivingVia
--   - do @deriving Num via RTB.RealtimeBalanceAsNum SimpleRealtimeBalance Wad@
--
newtype (RealtimeBalance rtb lq) => RealtimeBalanceAsNum rtb lq = RealtimeBalanceAsNum rtb
instance (Liquidity lq, RealtimeBalance rtb lq) => Num (RealtimeBalanceAsNum rtb lq) where
    (+) (RealtimeBalanceAsNum a) (RealtimeBalanceAsNum b) = RealtimeBalanceAsNum $
        fromBalanceVector $ zipWith (+) (toBalanceVector a) (toBalanceVector b)
    (*) (RealtimeBalanceAsNum a) (RealtimeBalanceAsNum b) = RealtimeBalanceAsNum $
        fromBalanceVector $ zipWith (*) (toBalanceVector a) (toBalanceVector b)
    fromInteger x = RealtimeBalanceAsNum
        $ liquidityToRTB . fromInteger $ x
    signum (RealtimeBalanceAsNum x) = RealtimeBalanceAsNum $
        liquidityToRTB . signum . liquidityFromRTB $ x
    abs (RealtimeBalanceAsNum x) = RealtimeBalanceAsNum $
        liquidityToRTB . abs . liquidityFromRTB $ x
    negate (RealtimeBalanceAsNum x) = RealtimeBalanceAsNum $
        liquidityToRTB . negate . liquidityFromRTB $ x
