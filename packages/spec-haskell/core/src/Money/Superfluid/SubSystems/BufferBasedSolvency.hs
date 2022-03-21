{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}

module Money.Superfluid.SubSystems.BufferBasedSolvency
    ( BufferLiquidity (..)
    , BufferLiquidityTag
    , bufferLiquidityTag
    , mkBufferTappedLiquidity
    ) where

import           Data.Default
import           Data.Typeable

import           Money.Superfluid.Concepts.Liquidity
    ( AnyTappedLiquidityTag (..)
    , Liquidity
    , TappedLiquidity (..)
    , TappedLiquidityTag
    )
import           Money.Superfluid.Concepts.TaggedTypeable

-- | BufferLiquidityTag Type and Its Utilities
--
data BufferLiquidityTag

instance TaggedTypeable BufferLiquidityTag where proxyTag _ = "d"
instance TappedLiquidityTag BufferLiquidityTag
instance Liquidity lq => Show (BufferLiquidity lq) where show = show . getBufferLiquidity

bufferLiquidityTag :: Proxy BufferLiquidityTag
bufferLiquidityTag = Proxy @BufferLiquidityTag

mkBufferTappedLiquidity :: Liquidity lq => lq -> TappedLiquidity lq
mkBufferTappedLiquidity liq = TappedLiquidity (MkTappedLiquidityTag bufferLiquidityTag, liq)

-- | BufferLiquidity Type
--
newtype Liquidity lq => BufferLiquidity lq = BufferLiquidity { getBufferLiquidity :: lq }
    deriving Default
