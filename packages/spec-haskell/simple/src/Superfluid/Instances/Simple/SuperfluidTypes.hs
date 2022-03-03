{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Superfluid.Instances.Simple.SuperfluidTypes
    ( Wad
    , toWad
    , wad4humanN
    , wad4human
    , SimpleTimestamp
    , SimpleRealtimeBalance
    ) where

import           Data.Default
import           Text.Printf                         (printf)

import           Superfluid.Concepts.BaseTypes       (Liquidity, Timestamp)
import           Superfluid.Concepts.RealtimeBalance (RealtimeBalance (..), RealtimeBalanceAsNum (..))

-- ============================================================================
-- Wad type:
--   * 18 decimal digit fixed-precision integer
--   * an instance of Liquidity
--
newtype Wad = Wad Integer deriving (Eq, Ord, Num, Default)

toWad :: (RealFrac a) => a -> Wad
toWad x = Wad (round $ x * (10 ^ (18::Integer)))

wad4humanN :: Wad -> Integer -> String -- TODO use Nat?
wad4humanN (Wad wad) n
    | n >= 0 && n <= 18 = printf
        ("%0."++(show n)++"f")
        ((fromIntegral wad / (10 ^ (18::Integer))) :: Double)
    | otherwise = error "Invalid parameter"

wad4human :: Wad -> String
wad4human wad = wad4humanN wad 4

instance Show Wad where
    show = wad4human

instance Liquidity Wad where

-- ============================================================================
-- SimpleTimestamp Base Type
--
newtype SimpleTimestamp = SimpleTimestamp Int deriving (Enum, Eq, Ord, Num, Real, Integral, Default)

instance Timestamp SimpleTimestamp

instance Show SimpleTimestamp where
    show (SimpleTimestamp t) = show t

-- ============================================================================
-- SimpleRealtimeBalance Base Type
--
data SimpleRealtimeBalance = SimpleRealtimeBalance
    { availableBalanceVal :: Wad
    , depositVal          :: Wad
    , owedDepositVal      :: Wad
    }
    deriving Num via RealtimeBalanceAsNum SimpleRealtimeBalance Wad

instance Default SimpleRealtimeBalance where
    def = SimpleRealtimeBalance { availableBalanceVal = def, depositVal = def, owedDepositVal = def }

instance RealtimeBalance SimpleRealtimeBalance Wad where
    availableBalance = availableBalanceVal
    toBalanceVector x = map (flip id x) [availableBalanceVal, depositVal, owedDepositVal]
    fromBalanceVector v = if length v == 3
        then SimpleRealtimeBalance (v!!0) (v!!1) (v!!2)
        else error "wrong balance vector"
    liquidityToRTB x = SimpleRealtimeBalance x o o where o = fromInteger(0)

instance Show SimpleRealtimeBalance where
    show (SimpleRealtimeBalance avb d od) =
        "("
        ++ show avb ++ ", "
        ++ show d ++ "@d, "
        ++ show od ++ "@od"
        ++ ")"
