{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeApplications           #-}

module Superfluid.Instances.Simple.SuperfluidTypes
    ( Wad (..)
    , toWad
    , wad4humanN
    , wad4human
    , SimpleTimestamp
    , SimpleRealtimeBalance
    ) where

import           Data.Default
import           Text.Printf                                 (printf)

import           Superfluid.Concepts.BaseTypes               (Liquidity, Timestamp)
import           Superfluid.Concepts.RealtimeBalance
    ( RealtimeBalance (..)
    , RealtimeBalanceAsNum (..)
    , liquidityRequiredForRTB
    )
--
import qualified Superfluid.Agreements.ConstantFlowAgreement as CFA

-- ============================================================================
-- Wad type:
--   * 18 decimal digit fixed-precision integer
--   * an instance of Liquidity
--
newtype Wad = Wad Integer deriving (Eq, Ord, Num, Default, Liquidity)

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

-- ============================================================================
-- SimpleTimestamp Base Type
--
newtype SimpleTimestamp = SimpleTimestamp Int deriving (Enum, Eq, Ord, Num, Real, Integral, Default, Timestamp)

instance Show SimpleTimestamp where
    show (SimpleTimestamp t) = show t

-- ============================================================================
-- SimpleRealtimeBalance Base Type
--
data SimpleRealtimeBalance = SimpleRealtimeBalance
    { untappedLiquidityVal :: Wad
    , depositVal           :: Wad
    , owedDepositVal       :: Wad
    }
    deriving Num via RealtimeBalanceAsNum SimpleRealtimeBalance Wad

instance Default SimpleRealtimeBalance where
    def = SimpleRealtimeBalance { untappedLiquidityVal = def, depositVal = def, owedDepositVal = def }

instance RealtimeBalance SimpleRealtimeBalance Wad where
    liquidityVectorFromRTB rtb = map (flip id rtb) [untappedLiquidityVal, depositVal, owedDepositVal]
    liquidityVectorToRTB vec = if length vec == 3
        then SimpleRealtimeBalance (vec!!0) (vec!!1) (vec!!2)
        else error "wrong balance vector"
    typedLiquidityVectorToRTB uliq tvec = SimpleRealtimeBalance uliq d od
        where d = foldr (+) def $ map CFA.getBufferLiquidity tvec
              od = def

instance Show SimpleRealtimeBalance where
    show rtb = (show . liquidityRequiredForRTB $ rtb) ++ (detail rtb) where
        detail (SimpleRealtimeBalance uliq d od) = " ("
            ++ show uliq ++ ", "
            ++ show d ++ "@d, "
            ++ show od ++ "@od"
            ++ ")"
