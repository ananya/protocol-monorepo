{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Money.Superfluid.Instances.Simple.SuperfluidTypes
    ( module Money.Superfluid.Concepts.Liquidity
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
    ) where

import           Data.Binary
import           Data.Default
import           GHC.Generics                                    (Generic)
import           Text.Printf                                     (printf)

import           Money.Superfluid.Concepts.Liquidity
import           Money.Superfluid.Concepts.RealtimeBalance
--
import qualified Money.Superfluid.SubSystems.BufferBasedSolvency as BBS

-- ============================================================================
-- Wad type:
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
-- SimpleTimestamp Base Type
--
newtype SimpleTimestamp = SimpleTimestamp Int
    deriving newtype (Enum, Eq, Ord, Num, Real, Integral, Default, Binary, Timestamp)

instance Show SimpleTimestamp where
    show (SimpleTimestamp t) = show t ++ "s"

-- ============================================================================
-- SimpleWadRate Base Type
--
newtype SimpleWadRate = SimpleWadRate Wad
    deriving newtype (Default, Num, Eq, Ord, Binary, Liquidity)

instance LiquidityVelocity SimpleWadRate Wad SimpleTimestamp where
    liquidityPerTimeUnit = SimpleWadRate
    liquidityTimesTimeUnit (SimpleWadRate wad) = wad
    lqvXts liqv = (* liquidityTimesTimeUnit liqv) . (fromInteger . toInteger)

instance Show SimpleWadRate where
     show liqv = (show . liquidityTimesTimeUnit $ liqv) ++ "/s"

-- ============================================================================
-- SimpleRealtimeBalance Base Type
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
        -- TODO: reduce it to a single loop
        where d = foldr ((+) . getLiquidityOfType BBS.bufferLiquidityType) def tvec
              od = def
