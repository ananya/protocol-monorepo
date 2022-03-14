{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Money.Superfluid.Agreements.ConstantFlowAgreement
    ( CFAContractData (..)
    , CFAAccountData (..)
    , updateFlow
    ) where

import           Text.Printf

import           Money.Superfluid.Concepts.AccountingUnit
import           Money.Superfluid.Concepts.Agreement
    ( AgreementAccountData (..)
    , AgreementContractData
    , AgreementData
    )
import           Money.Superfluid.Concepts.Liquidity
    ( LiquidityVelocity (..)
    , UntappedLiquidity (..)
    , untypeLiquidity
    )
import           Money.Superfluid.Concepts.RealtimeBalance       (RealtimeBalance (..), TypedLiquidityVector (..))
import           Money.Superfluid.Concepts.TaggedTypeable
--
import qualified Money.Superfluid.SubSystems.BufferBasedSolvency as BBS


-- ============================================================================
-- | CFAContractData Type
--
data AccountingUnit au => CFAContractData au = CFAContractData
    { flowLastUpdatedAt :: AU_TS au
    , flowRate          :: AU_LQV au
    , flowBuffer        :: BBS.BufferLiquidity (AU_LQ au)
    }
    deriving AgreementData
instance AccountingUnit au => TaggedTypeable (CFAContractData au) where typeTag _ = "CFA#"

instance AccountingUnit au => Show (CFAContractData au) where
    show x = printf "{ flowLastUpdatedAt = %s, flowRate = %s, flowBuffer = %s }"
        (show $ flowLastUpdatedAt x) (show $ flowRate x) (show $ flowBuffer x)

instance AccountingUnit au => AgreementContractData (CFAContractData au) au

-- ============================================================================
-- | CFAAccountData Type (is AgreementAccountData)
--
data AccountingUnit au => CFAAccountData au = CFAAccountData
    { settledAt                :: AU_TS au
    , settledUntappedLiquidity :: UntappedLiquidity (AU_LQ au)
    , settledBufferLiquidity   :: BBS.BufferLiquidity (AU_LQ au)
    , netFlowRate              :: AU_LQV au
    }
    deriving AgreementData
instance AccountingUnit au => TaggedTypeable (CFAAccountData au) where typeTag _ = "CFA."

instance AccountingUnit au => AgreementAccountData (CFAAccountData au) au where
    providedBalanceOfAgreement CFAAccountData
        { settledAt = t_s
        , settledUntappedLiquidity = (UntappedLiquidity uliq_s)
        , settledBufferLiquidity = (BBS.BufferLiquidity buf_s)
        , netFlowRate = r
        } t =
        typedLiquidityVectorToRTB $ TypedLiquidityVector
            ( UntappedLiquidity $ (r ~~* (t - t_s)) + uliq_s )
            [ BBS.mkBufferTappedLiquidity buf_s ]

instance AccountingUnit au => Show (CFAAccountData au) where
    show x = printf "{ t = %s, uliq = %s, net = %s, buf = %s}"
        (show $ settledAt x)
        (show $ untypeLiquidity $ settledUntappedLiquidity x)
        (show $ netFlowRate x)
        (show $ settledBufferLiquidity x)

-- ============================================================================
-- CFA Operations
--
updateFlow :: AccountingUnit au
    -- (cfaACD, senderAAD, receiverAAD)
    => (CFAContractData au, CFAAccountData au, CFAAccountData au)
    -- newFlowRate, newFlowBuffer, t
    -> AU_LQV au -> BBS.BufferLiquidity (AU_LQ au) -> AU_TS au
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
