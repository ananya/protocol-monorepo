{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}

module Money.Superfluid.SubSystems.BufferBasedSolvency where

import           Data.Default
import           Data.Typeable

import           Money.Superfluid.Concepts.Liquidity (Liquidity, LiquidityType (..), TappedLiquidity (..))

newtype Liquidity lq => BufferLiquidity lq = BufferLiquidity { getBufferLiquidity :: lq }
    deriving (Typeable, Default)
instance Liquidity lq => Show (BufferLiquidity lq) where
    show = show . getBufferLiquidity

bufferLiquidityType :: LiquidityType
bufferLiquidityType = LiquidityType (typeRep (Proxy @BufferLiquidity)) "d"

mkBufferTappedLiquidity :: Liquidity lq => lq -> TappedLiquidity lq
mkBufferTappedLiquidity liq = TappedLiquidity liq bufferLiquidityType
