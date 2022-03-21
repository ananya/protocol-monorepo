{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}

module Money.Superfluid.System.Serialization
    ( Putter (..)
    , Getter (..)
    , Serializable (..)
    , Serialized (..)
    ) where

import           Data.Proxy

import           Money.Superfluid.Concepts.Liquidity                      (UntappedLiquidity (..), untypeLiquidity)
import           Money.Superfluid.Concepts.SuperfluidTypes                (SuperfluidTypes (..))
import           Money.Superfluid.Concepts.TaggedTypeable                 (TaggedTypeable)
--
import qualified Money.Superfluid.SubSystems.BufferBasedSolvency          as BBS
--
import qualified Money.Superfluid.Agreements.ConstantFlowAgreement        as CFA
import qualified Money.Superfluid.Agreements.TransferableBalanceAgreement as TBA


class (Monad srl, SuperfluidTypes sft) => Putter srl sft | srl -> sft where
    putLQ :: SFT_LQ sft -> srl ()
    putTS :: SFT_TS sft -> srl ()
    putLQV :: SFT_LQV sft -> srl ()

class (Monad srl, SuperfluidTypes sft) => Getter srl sft | srl -> sft where
    getLQ :: srl (SFT_LQ sft)
    getTS :: srl (SFT_TS sft)
    getLQV :: srl (SFT_LQV sft)

class (TaggedTypeable a, SuperfluidTypes sft) => Serializable a sft | a -> sft where
    putter :: Putter srl sft => a -> srl ()
    getter :: Getter srl sft => Proxy a -> srl a

class SuperfluidTypes sft => Serialized s sft | s -> sft where
    runGetter :: Serializable a sft => Proxy a -> s -> a
    runPutter :: Serializable a sft => a -> s

-- ============================================================================
-- | Known Serializable Instances
--
instance SuperfluidTypes sft => Serializable (TBA.TBAAccountData sft) sft where
    putter s = do
        putTS (TBA.settledAt s)
        putLQ (untypeLiquidity $ TBA.liquidity s)
    getter _ = do
        t <- getTS
        l <- getLQ
        return TBA.TBAAccountData
            { TBA.settledAt = t
            , TBA.liquidity = UntappedLiquidity l
            }

instance SuperfluidTypes sft => Serializable (CFA.CFAContractData sft) sft where
    putter s = do
        putTS (CFA.flowLastUpdatedAt s)
        putLQV (CFA.flowRate s)
        putLQ (BBS.getBufferLiquidity $ CFA.flowBuffer s)
    getter _ = do
        t <- getTS
        r <- getLQV
        b <- getLQ
        return CFA.CFAContractData
            { CFA.flowLastUpdatedAt = t
            , CFA.flowRate = r
            , CFA.flowBuffer = BBS.BufferLiquidity b
            }

instance SuperfluidTypes sft => Serializable (CFA.CFAAccountData sft) sft where
    putter s = do
        putTS (CFA.settledAt s)
        putLQ (untypeLiquidity $ CFA.settledUntappedLiquidity s)
        putLQ (BBS.getBufferLiquidity $ CFA.settledBufferLiquidity s)
        putLQV (CFA.netFlowRate s)
    getter _ = do
        t <- getTS
        l <- getLQ
        b <- getLQ
        r <- getLQV
        return CFA.CFAAccountData
            { CFA.settledAt = t
            , CFA.settledUntappedLiquidity = UntappedLiquidity l
            , CFA.settledBufferLiquidity = BBS.BufferLiquidity b
            , CFA.netFlowRate = r
            }
