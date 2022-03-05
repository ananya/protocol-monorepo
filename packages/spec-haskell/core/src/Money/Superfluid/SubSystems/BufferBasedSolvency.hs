{-# LANGUAGE TypeApplications #-}

module Money.Superfluid.SubSystems.BufferBasedSolvency where

import           Data.Typeable

import           Money.Superfluid.Concepts.BaseTypes (Liquidity, TappedLiquidity (..))

newtype Liquidity lq => BufferLiquidity lq = BufferLiquidity { getBufferLiquidity :: lq } deriving (Typeable)

bufferLiquidityType :: TypeRep
bufferLiquidityType = typeRep (Proxy @BufferLiquidity)

mkTappedBufferLiquidity :: Liquidity lq => lq -> TappedLiquidity lq
mkTappedBufferLiquidity uliq = TappedLiquidity uliq bufferLiquidityType
