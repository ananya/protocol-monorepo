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

newtype Liquidity lq => BufferLiquidity lq = BufferLiquidity { getBufferLiquidity :: lq }
    deriving Default
instance Liquidity lq => Show (BufferLiquidity lq) where show = show . getBufferLiquidity

data BufferLiquidityTag
instance TappedLiquidityTag BufferLiquidityTag
instance TaggedTypeable BufferLiquidityTag where typeTag _ = "d"

bufferLiquidityTag :: Proxy BufferLiquidityTag
bufferLiquidityTag = Proxy @BufferLiquidityTag
-- bufferLiquidityTag = TypeTag (typeRep $ Proxy @BufferLiquidity) "d"

mkBufferTappedLiquidity :: Liquidity lq => lq -> TappedLiquidity lq
mkBufferTappedLiquidity liq = TappedLiquidity liq (MkTappedLiquidityTag bufferLiquidityTag)
