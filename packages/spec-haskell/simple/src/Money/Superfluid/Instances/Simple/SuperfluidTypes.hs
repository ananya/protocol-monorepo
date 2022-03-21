{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

module Money.Superfluid.Instances.Simple.SuperfluidTypes
    ( module Money.Superfluid.Concepts.Liquidity
    -- SimpleAddress
    , SimpleAddress
    , createSimpleAddress
    -- Wad
    , Wad (..)
    , toWad
    , wad4humanN
    , wad4human
    -- SimpleWadRate
    , SimpleWadRate
    -- SimpleTimestamp
    , SimpleTimestamp
    -- SimpleRealtimeBalance
    , module Money.Superfluid.Concepts.RealtimeBalance
    , SimpleRealtimeBalance
    -- SimpleSuperfluidTypes
    , SimpleSuperfluidTypes
    ) where

import           Data.Binary
import           Data.Char                                       (isAlpha)
import           Data.Default
import           GHC.Generics                                    (Generic)
import           Text.Printf                                     (printf)

import           Money.Superfluid.Concepts.Liquidity
import           Money.Superfluid.Concepts.RealtimeBalance
import           Money.Superfluid.Concepts.SuperfluidTypes
--
import qualified Money.Superfluid.SubSystems.BufferBasedSolvency as BBS

-- ============================================================================
-- Wad Type:
--   * 18 decimal digit fixed-precision integer
--   * an instance of Liquidity
--
newtype Wad = Wad Integer
    deriving newtype (Eq, Ord, Num, Default, Binary, Liquidity)

toWad :: (RealFrac a) => a -> Wad
toWad x = Wad (round $ x * (10 ^ (18::Integer)))

wad4humanN :: Wad -> Integer -> String -- TODO use Nat?
wad4humanN (Wad wad) n
    | n >= 0 && n <= 18 = printf
        ("%0." ++ show n ++ "f")
        ((fromIntegral wad / (10 ^ (18::Integer))) :: Double)
    | otherwise = error "Invalid parameter"

wad4human :: Wad -> String
wad4human wad = wad4humanN wad 4

instance Show Wad where
    show = wad4human

-- ============================================================================
-- SimpleTimestamp Type
--
newtype SimpleTimestamp = SimpleTimestamp Int
    deriving newtype (Enum, Eq, Ord, Num, Real, Integral, Default, Binary)
    deriving anyclass (Timestamp)

instance Show SimpleTimestamp where
    show (SimpleTimestamp t) = show t ++ "s"

-- ============================================================================
-- SimpleWadRate Type
--
newtype SimpleWadRate = SimpleWadRate Wad
    deriving newtype (Default, Num, Eq, Ord, Binary)
    deriving anyclass (Liquidity)

instance LiquidityVelocity SimpleWadRate Wad SimpleTimestamp where
    liquidityPerTimeUnit = SimpleWadRate
    liquidityTimesTimeUnit (SimpleWadRate wad) = wad
    lqvXts liqv = (* liquidityTimesTimeUnit liqv) . (fromInteger . toInteger)

instance Show SimpleWadRate where
     show liqv = (show . liquidityTimesTimeUnit $ liqv) ++ "/s"

-- ============================================================================
-- SimpleRealtimeBalance Type
--
data SimpleRealtimeBalance = SimpleRealtimeBalance
    { untappedLiquidityVal :: Wad
    , depositVal           :: Wad
    , owedDepositVal       :: Wad
    }
    deriving stock (Generic)
    deriving anyclass (Binary)
    deriving Num via RealtimeBalanceAsNum SimpleRealtimeBalance Wad
    deriving Show via RealtimeBalanceAsShow SimpleRealtimeBalance Wad

instance Default SimpleRealtimeBalance where
    def = SimpleRealtimeBalance { untappedLiquidityVal = def, depositVal = def, owedDepositVal = def }

instance RealtimeBalance SimpleRealtimeBalance Wad where
    rawLiquidityVectorFromRTB rtb = map (`id` rtb) [untappedLiquidityVal, depositVal, owedDepositVal]

    typedLiquidityVectorFromRTB rtb = TypedLiquidityVector
        ( UntappedLiquidity $ untappedLiquidityVal rtb)
        [ BBS.mkBufferTappedLiquidity $ depositVal rtb
        ]

    untappedLiquidityToRTB uliq = SimpleRealtimeBalance uliq def def

    untypedLiquidityVectorToRTB (UntypedLiquidityVector uliq uvec) = if length uvec == 2
        then SimpleRealtimeBalance uliq (head uvec) (uvec!!1)
        else error "Wrong untyped liquidity vector length"

    typedLiquidityVectorToRTB (TypedLiquidityVector (UntappedLiquidity uliq) tvec) = SimpleRealtimeBalance uliq d od
        where d = foldr ((+) . (`getLiquidityOfType` BBS.bufferLiquidityTag)) def tvec
              od = def

-- ============================================================================
-- SimpleAddress Type
--
-- Note: It must consist of only alphabetical letters
--
newtype SimpleAddress = SimpleAddress String
    deriving newtype (Eq, Ord, Binary, Show)
    deriving anyclass (Address)

-- SimpleAddress public constructor
createSimpleAddress :: String -> Maybe SimpleAddress
createSimpleAddress a = if all isAlpha a then Just $ SimpleAddress a else Nothing

data SimpleSuperfluidTypes

instance SuperfluidTypes SimpleSuperfluidTypes where
    type SFT_LQ SimpleSuperfluidTypes = Wad
    type SFT_TS SimpleSuperfluidTypes = SimpleTimestamp
    type SFT_LQV SimpleSuperfluidTypes = SimpleWadRate
    type SFT_RTB SimpleSuperfluidTypes = SimpleRealtimeBalance
    type SFT_ADDR SimpleSuperfluidTypes = SimpleAddress
