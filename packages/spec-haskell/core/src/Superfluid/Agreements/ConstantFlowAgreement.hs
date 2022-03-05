{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Superfluid.Agreements.ConstantFlowAgreement
    ( CFAContractData (..)
    , CFAAccountData (..)
    , mkBufferLiquidity
    , getBufferLiquidity
    , updateFlow
    ) where
import           Data.Default
import           Text.Printf

import           Superfluid.Concepts.AccountingUnit        (AccountingUnit (..))
import           Superfluid.Concepts.Agreement             (AgreementAccountData (..), AgreementContractData)
import           Superfluid.Concepts.RealtimeBalance       (typedLiquidityVectorToRTB)
import           Superfluid.SubSystems.BufferBasedSolvency


-- ============================================================================
-- | CFAContractData Type
--
data AccountingUnit au => CFAContractData au = CFAContractData
    { flowLastUpdatedAt :: AU_TS au
    , flowRate          :: AU_LQ au
    , flowBuffer        :: AU_LQ au
    }

instance AccountingUnit au => Default (CFAContractData au) where
    def = CFAContractData { flowLastUpdatedAt = def, flowRate = def, flowBuffer = def }

instance AccountingUnit au => Show (CFAContractData au) where
    show x = printf "{ flowLastUpdatedAt = %s, flowRate = %s, flowBuffer = %s }"
        (show $ flowLastUpdatedAt x) (show $ flowRate x) (show $ flowBuffer x)

instance AccountingUnit au => AgreementContractData (CFAContractData au) au

-- ============================================================================
-- | CFAAccountData Type (is AgreementAccountData)
--
data AccountingUnit au => CFAAccountData au = CFAAccountData
    { settledAt                :: AU_TS au
    , settledUntappedLiquidity :: AU_LQ au
    , settledBufferLiquidity   :: AU_LQ au
    , netFlowRate              :: AU_LQ au
    }

instance AccountingUnit au => Default (CFAAccountData au) where
    def = CFAAccountData
        { settledAt = def
        , settledUntappedLiquidity = def
        , settledBufferLiquidity = def
        , netFlowRate = def
        }

instance AccountingUnit au => AgreementAccountData (CFAAccountData au) au where
    providedBalanceOfAgreement CFAAccountData
        { netFlowRate = r
        , settledUntappedLiquidity = uliq_s
        , settledBufferLiquidity = buf_s
        , settledAt = t_s
        } t =
        typedLiquidityVectorToRTB
            ((fromInteger.toInteger)(t - t_s) * r + uliq_s)
            [ mkBufferLiquidity buf_s ]

instance AccountingUnit au => Show (CFAAccountData au) where
    show x = printf "{ at = %s, uliq = %s, buf = %s net = %s }"
        (show $ settledAt x)
        (show $ settledUntappedLiquidity x)
        (show $ settledBufferLiquidity x)
        (show $ netFlowRate x)

-- ============================================================================
-- CFA Operations
--
updateFlow :: AccountingUnit au
    -- (cfaACD, senderAAD, receiverAAD)
    => (CFAContractData au, CFAAccountData au, CFAAccountData au)
    -- newFlowRate, newFlowBuffer, t
    -> AU_LQ au -> AU_LQ au -> AU_TS au
    -- (cfaACD', senderAAD', receiverAAD')
    -> (CFAContractData au, CFAAccountData au, CFAAccountData au)
updateFlow (cfaACD, senderAAD, receiverAAD) newFlowRate newFlowBuffer t =
    ( CFAContractData
        { flowLastUpdatedAt = t
        , flowRate = newFlowRate
        , flowBuffer = newFlowBuffer
        }
    , updateFlowRate senderAAD (negate flowRateDelta) t
    , updateFlowRate receiverAAD flowRateDelta t
    )
    where
    flowRateDelta = newFlowRate - (flowRate cfaACD)
    flowBufferDelta = newFlowBuffer - (flowBuffer cfaACD)
    updateFlowRate CFAAccountData
        { netFlowRate = r
        , settledUntappedLiquidity = uliq_s
        , settledBufferLiquidity = buf_s
        , settledAt = t_s
        } r_delta t_s'
        = CFAAccountData
        { netFlowRate = r + r_delta
        , settledUntappedLiquidity = uliq_s + (fromInteger.toInteger)(t - t_s) * r - flowBufferDelta
        , settledBufferLiquidity = buf_s + flowBufferDelta
        , settledAt = t_s'
        }
