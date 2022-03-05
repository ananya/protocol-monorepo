{-# LANGUAGE DerivingVia                #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Money.Superfluid.Instances.Simple.SuperfluidTypes
    ( Wad (..)
    , toWad
    , wad4humanN
    , wad4human
    , SimpleTimestamp
    , SimpleRealtimeBalance
    ) where

import           Data.Default
import           Text.Printf                                     (printf)

import           Money.Superfluid.Concepts.Liquidity             (Liquidity, Timestamp, getLiquidityOfType)
import           Money.Superfluid.Concepts.RealtimeBalance
    ( LiquidityVector (..)
    , RealtimeBalance (..)
    , RealtimeBalanceAsNum (..)
    , RealtimeBalanceAsShow (..)
    )
--
import qualified Money.Superfluid.SubSystems.BufferBasedSolvency as BBS

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
newtype SimpleTimestamp = SimpleTimestamp Int
    deriving (Enum, Eq, Ord, Num, Real, Integral, Default, Timestamp)

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
    deriving Show via RealtimeBalanceAsShow SimpleRealtimeBalance Wad

instance Default SimpleRealtimeBalance where
    def = SimpleRealtimeBalance { untappedLiquidityVal = def, depositVal = def, owedDepositVal = def }

instance RealtimeBalance SimpleRealtimeBalance Wad where
    rawLiquidityVectorFromRTB rtb = map (flip id rtb) [untappedLiquidityVal, depositVal, owedDepositVal]

    typedLiquidityVectorFromRTB rtb = TypedLiquidityVector
        (untappedLiquidityVal rtb)
        [ BBS.mkBufferTypedLiquidity $ depositVal rtb
        ]

    untappedLiquidityToRTB uliq = SimpleRealtimeBalance uliq def def

    liquidityVectorToRTB (UntypedLiquidityVector uliq uvec) = if length uvec == 2
        then SimpleRealtimeBalance uliq (uvec!!0) (uvec!!1)
        else error "Wrong untyped liquidity vector length"
    liquidityVectorToRTB (TypedLiquidityVector uliq tvec) = SimpleRealtimeBalance uliq d od
        -- TODO: reduce it to a single loop
        where d = foldr (+) def $ map (getLiquidityOfType BBS.bufferLiquidityType) tvec
              od = def
