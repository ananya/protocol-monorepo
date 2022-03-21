{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Money.Superfluid.Agreements.ConstantFlowAgreement
    ( CFAContractData (..)
    , CFAAccountData (..)
    , updateFlow
    ) where

import           Data.Default
import           Text.Printf

import           Money.Superfluid.Concepts.Agreement             (AgreementAccountData (..), AgreementContractData)
import           Money.Superfluid.Concepts.Liquidity
    ( LiquidityVelocity (..)
    , UntappedLiquidity (..)
    , untypeLiquidity
    )
import           Money.Superfluid.Concepts.RealtimeBalance       (RealtimeBalance (..), TypedLiquidityVector (..))
import           Money.Superfluid.Concepts.SuperfluidTypes
import           Money.Superfluid.Concepts.TaggedTypeable
--
import qualified Money.Superfluid.SubSystems.BufferBasedSolvency as BBS


-- ============================================================================
-- | CFAContractData Type
--
data SuperfluidTypes sft => CFAContractData sft = CFAContractData
    { flowLastUpdatedAt :: SFT_TS sft
    , flowRate          :: SFT_LQV sft
    , flowBuffer        :: BBS.BufferLiquidity (SFT_LQ sft)
    }
instance SuperfluidTypes sft => TaggedTypeable (CFAContractData sft) where proxyTag _ = "CFA#"
instance SuperfluidTypes sft => Default (CFAContractData sft) where
    def = CFAContractData
        { flowLastUpdatedAt = def
        , flowRate = def
        , flowBuffer = def
        }

instance SuperfluidTypes sft => Show (CFAContractData sft) where
    show x = printf "{ flowLastUpdatedAt = %s, flowRate = %s, flowBuffer = %s }"
        (show $ flowLastUpdatedAt x) (show $ flowRate x) (show $ flowBuffer x)

instance SuperfluidTypes sft => AgreementContractData (CFAContractData sft) sft

-- ============================================================================
-- | CFAAccountData Type (is AgreementAccountData)
--
data SuperfluidTypes sft => CFAAccountData sft = CFAAccountData
    { settledAt                :: SFT_TS sft
    , settledUntappedLiquidity :: UntappedLiquidity (SFT_LQ sft)
    , settledBufferLiquidity   :: BBS.BufferLiquidity (SFT_LQ sft)
    , netFlowRate              :: SFT_LQV sft
    }
instance SuperfluidTypes sft => TaggedTypeable (CFAAccountData sft) where proxyTag _ = "CFA"
instance SuperfluidTypes sft => Default (CFAAccountData sft) where
    def = CFAAccountData
        { settledAt = def
        , settledUntappedLiquidity = def
        , settledBufferLiquidity = def
        , netFlowRate = def }

instance SuperfluidTypes sft => AgreementAccountData (CFAAccountData sft) sft where
    providedBalanceOfAgreement CFAAccountData
        { settledAt = t_s
        , settledUntappedLiquidity = (UntappedLiquidity uliq_s)
        , settledBufferLiquidity = (BBS.BufferLiquidity buf_s)
        , netFlowRate = r
        } t =
        typedLiquidityVectorToRTB $ TypedLiquidityVector
            ( UntappedLiquidity $ (r ~~* (t - t_s)) + uliq_s )
            [ BBS.mkBufferTappedLiquidity buf_s ]

instance SuperfluidTypes sft => Show (CFAAccountData sft) where
    show x = printf "{ t = %s, uliq = %s, net = %s, buf = %s}"
        (show $ settledAt x)
        (show $ untypeLiquidity $ settledUntappedLiquidity x)
        (show $ netFlowRate x)
        (show $ settledBufferLiquidity x)

-- ============================================================================
-- CFA Operations
--
updateFlow :: SuperfluidTypes sft
    -- (cfaACD, senderAAD, receiverAAD)
    => (CFAContractData sft, CFAAccountData sft, CFAAccountData sft)
    -- newFlowRate, newFlowBuffer, t
    -> SFT_LQV sft -> BBS.BufferLiquidity (SFT_LQ sft) -> SFT_TS sft
    -- (cfaACD', senderAAD', receiverAAD')
    -> (CFAContractData sft, CFAAccountData sft, CFAAccountData sft)
updateFlow (cfaACD, senderAAD, receiverAAD) newFlowRate (BBS.BufferLiquidity newFlowBuffer) t =
    ( CFAContractData
        { flowLastUpdatedAt = t
        , flowRate = newFlowRate
        , flowBuffer = BBS.BufferLiquidity newFlowBuffer
        }
    , updateFlowRate senderAAD (negate flowRateDelta) t
    , updateFlowRate receiverAAD flowRateDelta t
    )
    where
    flowRateDelta = newFlowRate - flowRate cfaACD
    flowBufferDelta = newFlowBuffer - (BBS.getBufferLiquidity . flowBuffer $ cfaACD)
    updateFlowRate CFAAccountData
        { netFlowRate = r
        , settledUntappedLiquidity = (UntappedLiquidity uliq_s)
        , settledBufferLiquidity = (BBS.BufferLiquidity buf_s)
        , settledAt = t_s
        } r_delta t_s'
        = CFAAccountData
        { netFlowRate = r + r_delta
        , settledUntappedLiquidity = UntappedLiquidity $ uliq_s + (r ~~* (t - t_s)) - flowBufferDelta
        , settledBufferLiquidity = BBS.BufferLiquidity $ buf_s + flowBufferDelta
        , settledAt = t_s'
        }
