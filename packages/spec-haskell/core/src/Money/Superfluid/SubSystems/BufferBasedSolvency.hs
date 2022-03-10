{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}

module Money.Superfluid.SubSystems.BufferBasedSolvency where

import           Data.Default
import           Data.Typeable

import           Money.Superfluid.Concepts.Liquidity (Liquidity, LiquidityTag (..), TappedLiquidity (..))

newtype Liquidity lq => BufferLiquidity lq = BufferLiquidity { getBufferLiquidity :: lq }
    deriving (Typeable, Default)
instance Liquidity lq => Show (BufferLiquidity lq) where
    show = show . getBufferLiquidity

bufferLiquidityTag :: LiquidityTag
bufferLiquidityTag = LiquidityTag (typeRep (Proxy @BufferLiquidity)) "d"

mkBufferTappedLiquidity :: Liquidity lq => lq -> TappedLiquidity lq
mkBufferTappedLiquidity liq = TappedLiquidity liq bufferLiquidityTag
