{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Superfluid.Agreements.ConstantFlowAgreement
    ( CFAContractData
    , CFAAccountData
    , getNetFlowRate
    , updateFlow
    ) where

import           Data.Default
import           Text.Printf

import           Superfluid.Concepts.AccountingUnit  (AccountingUnit (..))
import           Superfluid.Concepts.Agreement       (AgreementAccountData (..), AgreementContractData)
import           Superfluid.Concepts.BaseTypes       (integralToLiquidity)
import           Superfluid.Concepts.RealtimeBalance (liquidityToRTB)


-- ============================================================================
-- | CFAContractData Type
--
data AccountingUnit au => CFAContractData au = CFAContractData
    { flowLastUpdatedAt :: AU_TS au
    , flowRate          :: AU_LQ au
    }

instance AccountingUnit au => Default (CFAContractData au) where
    def = CFAContractData { flowLastUpdatedAt = def, flowRate = def }

instance AccountingUnit au => Show (CFAContractData au) where
    show x = printf "{ flowLastUpdatedAt = %s, flowRate = %s }"
        (show $ flowLastUpdatedAt x) (show $ flowRate x)

instance AccountingUnit au => AgreementContractData (CFAContractData au) au

-- ============================================================================
-- | CFAAccountData Type (is AgreementAccountData)
--
data AccountingUnit au => CFAAccountData au = CFAAccountData
    { settledAt      :: AU_TS au
    , settledBalance :: AU_LQ au
    , netFlowRate    :: AU_LQ au
    }

instance AccountingUnit au => Default (CFAAccountData au) where
    def = CFAAccountData { settledAt = def, settledBalance = def, netFlowRate = def }

instance AccountingUnit au => AgreementAccountData (CFAAccountData au) au where
    providedBalanceOfAgreement CFAAccountData { netFlowRate = r, settledBalance = b_s, settledAt = t_s } t =
        liquidityToRTB $ integralToLiquidity(t - t_s) * r + b_s

instance AccountingUnit au => Show (CFAAccountData au) where
    show x = printf "{ settledAt = %s, settledBalance = %s, netFlowRate = %s }"
        (show $ settledAt x) (show $ settledBalance x) (show $ netFlowRate x)

-- ============================================================================
-- CFA Operations
--
getNetFlowRate :: AccountingUnit au => CFAAccountData au -> AU_LQ au
getNetFlowRate = netFlowRate

updateFlow :: AccountingUnit au
    -- (cfaACD, senderAAD, receiverAAD)
    => (CFAContractData au, CFAAccountData au, CFAAccountData au)
    -- newFlowRate, t
    -> AU_LQ au -> AU_TS au
    -- (cfaACD', senderAAD', receiverAAD')
    -> (CFAContractData au, CFAAccountData au, CFAAccountData au)
updateFlow (cfaACD, senderAAD, receiverAAD) newFlowRate t =
    ( CFAContractData
        { flowLastUpdatedAt = t
        , flowRate = newFlowRate
        }
    , updateFlowRate senderAAD (negate flowRateDelta) t
    , updateFlowRate receiverAAD flowRateDelta t
    )
    where
        flowRateDelta = newFlowRate - (flowRate cfaACD)
        updateFlowRate CFAAccountData { netFlowRate = r, settledBalance = b_s, settledAt = t_s } r_delta t_s' =
            CFAAccountData
                { netFlowRate = r + r_delta
                , settledBalance = b_s + integralToLiquidity(t - t_s) * r
                , settledAt = t_s'
                }
