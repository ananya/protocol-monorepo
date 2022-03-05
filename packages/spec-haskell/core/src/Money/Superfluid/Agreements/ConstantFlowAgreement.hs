{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}

module Money.Superfluid.Agreements.ConstantFlowAgreement
    ( CFAContractData (..)
    , CFAAccountData (..)
    , updateFlow
    ) where
import           Data.Default
import           Text.Printf

import           Money.Superfluid.Concepts.AccountingUnit        (AccountingUnit (..))
import           Money.Superfluid.Concepts.Agreement             (AgreementAccountData (..), AgreementContractData)
import           Money.Superfluid.Concepts.RealtimeBalance       (LiquidityVector (..), RealtimeBalance (..))
import qualified Money.Superfluid.SubSystems.BufferBasedSolvency as BBS


-- ============================================================================
-- | CFAContractData Type
--
data AccountingUnit au => CFAContractData au = CFAContractData
    { flowLastUpdatedAt :: AU_TS au
    , flowRate          :: AU_LQ au
    , flowBuffer        :: BBS.BufferLiquidity (AU_LQ au)
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
    , settledBufferLiquidity   :: BBS.BufferLiquidity (AU_LQ au)
    , netFlowRate              :: AU_LQ au
    }

instance AccountingUnit au => Default (CFAAccountData au) where
    def = CFAAccountData
        { settledAt = def
        , settledUntappedLiquidity = def
        , netFlowRate = def
        , settledBufferLiquidity = def
        }

instance AccountingUnit au => AgreementAccountData (CFAAccountData au) au where
    providedBalanceOfAgreement CFAAccountData
        { netFlowRate = r
        , settledUntappedLiquidity = uliq_s
        , settledBufferLiquidity = (BBS.BufferLiquidity buf_s)
        , settledAt = t_s
        } t =
        liquidityVectorToRTB $ TypedLiquidityVector
            ( (fromInteger.toInteger)(t - t_s) * r + uliq_s )
            [ BBS.mkBufferTypedLiquidity buf_s ]

instance AccountingUnit au => Show (CFAAccountData au) where
    show x = printf "{ t = %s, uliq = %s, net = %s, buf = %s}"
        (show $ settledAt x)
        (show $ settledUntappedLiquidity x)
        (show $ netFlowRate x)
        (show $ settledBufferLiquidity x)

-- ============================================================================
-- CFA Operations
--
updateFlow :: AccountingUnit au
    -- (cfaACD, senderAAD, receiverAAD)
    => (CFAContractData au, CFAAccountData au, CFAAccountData au)
    -- newFlowRate, newFlowBuffer, t
    -> AU_LQ au -> BBS.BufferLiquidity (AU_LQ au) -> AU_TS au
    -- (cfaACD', senderAAD', receiverAAD')
    -> (CFAContractData au, CFAAccountData au, CFAAccountData au)
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
    flowRateDelta = newFlowRate - (flowRate cfaACD)
    flowBufferDelta = newFlowBuffer - (BBS.getBufferLiquidity . flowBuffer $ cfaACD)
    updateFlowRate CFAAccountData
        { netFlowRate = r
        , settledUntappedLiquidity = uliq_s
        , settledBufferLiquidity = (BBS.BufferLiquidity buf_s)
        , settledAt = t_s
        } r_delta t_s'
        = CFAAccountData
        { netFlowRate = r + r_delta
        , settledUntappedLiquidity = uliq_s + (fromInteger.toInteger)(t - t_s) * r - flowBufferDelta
        , settledBufferLiquidity = BBS.BufferLiquidity $ buf_s + flowBufferDelta
        , settledAt = t_s'
        }
