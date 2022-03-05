module Superfluid.SubSystems.BufferBasedSolvency where

import           Data.Default

import           Superfluid.Concepts.BaseTypes (Liquidity, TappedLiquidity (..))

mkBufferLiquidity :: Liquidity lq => lq -> TappedLiquidity lq
mkBufferLiquidity uliq = TappedLiquidity uliq 42

getBufferLiquidity :: Liquidity lq => TappedLiquidity lq -> lq
getBufferLiquidity (TappedLiquidity uliq 42) = uliq
getBufferLiquidity _                         = def
