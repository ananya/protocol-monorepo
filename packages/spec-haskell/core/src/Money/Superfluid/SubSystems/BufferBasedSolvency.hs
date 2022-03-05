{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeApplications           #-}

module Money.Superfluid.SubSystems.BufferBasedSolvency where

import           Data.Default
import           Data.Typeable

import           Money.Superfluid.Concepts.Liquidity (Liquidity, LiquidityType (..), TypedLiquidity (..))

newtype Liquidity lq => BufferLiquidity lq = BufferLiquidity { getBufferLiquidity :: lq }
    deriving (Typeable, Default)
instance Liquidity lq => Show (BufferLiquidity lq) where
    show = show . getBufferLiquidity

bufferLiquidityType :: LiquidityType
bufferLiquidityType = LiquidityType (typeRep (Proxy @BufferLiquidity)) "d"

mkBufferTypedLiquidity :: Liquidity lq => lq -> TypedLiquidity lq
mkBufferTypedLiquidity liq = TappedLiquidity liq bufferLiquidityType
