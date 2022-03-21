{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Money.Superfluid.System.AccountTokenModel
    ( Account (..)
    , balanceOfAccountAt
    , sumAccounts
    , Token (..)
    ) where

import           Data.Default
import           Data.Kind                                                (Type)
import           Data.Maybe
import           Data.Proxy

import           Money.Superfluid.Concepts.Liquidity                      (LiquidityVelocity (..))
import           Money.Superfluid.Concepts.SuperfluidTypes                (SuperfluidTypes (..))
--
import           Money.Superfluid.Concepts.Agreement
    ( AgreementAccountData
    , AgreementContractData
    , AnyAgreementAccountData (..)
    , providedBalanceOfAnyAgreement
    )
--
import qualified Money.Superfluid.Agreements.ConstantFlowAgreement        as CFA
import qualified Money.Superfluid.Agreements.TransferableBalanceAgreement as TBA
--
import qualified Money.Superfluid.SubSystems.BufferBasedSolvency          as BBS
--
import qualified Money.Superfluid.System.Serialize                        as S


-- | SuperfluidTypes type class
--
-- Naming conventions:
--   * Type name: acc
--   * Type family name: SF_ACC
--   * Term name: *Account
class (SuperfluidTypes acc) => Account acc where
    addressOfAccount :: acc -> SFT_ADDR acc

    agreementOfAccount
        :: (AgreementAccountData aad acc, S.Serializable aad acc)
        => Proxy aad -> acc -> Maybe aad

    updateAgreementOfAccount
        :: (AgreementAccountData aad acc, S.Serializable aad acc)
        => acc -> aad -> SFT_TS acc -> acc

    accountTBA :: acc -> TBA.TBAAccountData acc
    accountTBA acc = fromMaybe def $ agreementOfAccount (Proxy @(TBA.TBAAccountData acc)) acc

    accountCFA :: acc -> CFA.CFAAccountData acc
    accountCFA acc = fromMaybe def $ agreementOfAccount (Proxy @(CFA.CFAAccountData acc)) acc

    agreementsOfAccount :: acc -> [AnyAgreementAccountData acc]
    agreementsOfAccount acc =
        [ MkAgreementAccountData $ accountTBA acc
        , MkAgreementAccountData $ accountCFA acc
        ]

    showAccountAt :: acc -> SFT_TS acc -> String

balanceOfAccountAt :: Account acc => acc -> SFT_TS acc -> SFT_RTB acc
balanceOfAccountAt holderAccount t = foldr
    ((+) . (`providedBalanceOfAnyAgreement` t))
    def
    (agreementsOfAccount holderAccount)

sumAccounts :: Account acc => [acc] -> SFT_TS acc -> SFT_RTB acc
sumAccounts alist t = foldr ((+) . (`balanceOfAccountAt` t)) def alist


-- ============================================================================
-- | Token Type Class
--
-- Naming conventions:
--   * Type name: tk
--   * Term name: *Token
--
-- Notes:
--
-- * Token is a monadic type, where all its functions run within the monadic context.
-- * Token provides:
--   * addressable account,
--   * and agreement (TBA/CFA/GDA) operations.
-- * Instructions for write operations are executed in `execTokenInstructions`.
--
class (Monad tk, Account (TK_ACC tk)) => Token tk where

    type TK_ACC tk :: Type

    --
    -- System operations
    --
    getCurrentTime :: tk (SFT_TS (TK_ACC tk))

    --
    -- Agreement operations
    --
    getAgreementContractData
        :: (AgreementContractData acd (TK_ACC tk), S.Serializable acd (TK_ACC tk))
        => Proxy acd -> [SFT_ADDR (TK_ACC tk)] -> tk (Maybe acd)

    putAgreementContractData
        :: (AgreementContractData acd (TK_ACC tk), S.Serializable acd (TK_ACC tk))
        => [SFT_ADDR (TK_ACC tk)] -> SFT_TS (TK_ACC tk) -> acd -> tk ()

    --
    -- Account operations
    --
    getAccount :: SFT_ADDR (TK_ACC tk) -> tk (TK_ACC tk)

    putAccount :: SFT_ADDR (TK_ACC tk) -> TK_ACC tk -> tk ()

    balanceOfAccount :: SFT_ADDR (TK_ACC tk) -> tk (SFT_RTB (TK_ACC tk))
    balanceOfAccount addr = do
        t <- getCurrentTime
        account <- getAccount addr
        return $ balanceOfAccountAt account t

    --
    -- TBA functions
    --
    mintLiquidity :: SFT_ADDR (TK_ACC tk) -> SFT_LQ (TK_ACC tk) -> tk ()
    mintLiquidity addr liquidity = do
        t <- getCurrentTime
        account <- getAccount addr
        let tbaAAD' = (TBA.mintLiquidity . accountTBA) account liquidity
        let account' = updateAgreementOfAccount account tbaAAD' t
        putAccount addr account'

    --
    -- CFA functions
    --
    calcFlowBuffer :: SFT_LQ (TK_ACC tk) -> tk (SFT_LQ (TK_ACC tk))

    getFlow :: SFT_ADDR (TK_ACC tk) -> SFT_ADDR (TK_ACC tk) -> tk (CFA.CFAContractData (TK_ACC tk))
    getFlow senderAddr receiverAddr = fromMaybe def <$> getAgreementContractData
        (Proxy @(CFA.CFAContractData (TK_ACC tk)))
        [senderAddr, receiverAddr]

    updateFlow :: SFT_ADDR (TK_ACC tk) -> SFT_ADDR (TK_ACC tk) -> SFT_LQ (TK_ACC tk) -> tk ()
    updateFlow senderAddr receiverAddr newFlowRate = do
        t <- getCurrentTime
        senderAccount <- getAccount senderAddr
        receiverAccount <- getAccount receiverAddr
        flowACD <- getFlow senderAddr receiverAddr
        flowBuffer <-  calcFlowBuffer newFlowRate
        let (flowACD', senderFlowAAD', receiverFlowAAD') = CFA.updateFlow
                (flowACD, accountCFA senderAccount, accountCFA receiverAccount)
                (liquidityPerTimeUnit newFlowRate) (BBS.BufferLiquidity flowBuffer) t
        let senderAccount' = updateAgreementOfAccount senderAccount senderFlowAAD' t
        let receiverAccount' = updateAgreementOfAccount receiverAccount receiverFlowAAD' t
        putAgreementContractData [senderAddr, receiverAddr] t flowACD'
        putAccount senderAddr senderAccount'
        putAccount receiverAddr receiverAccount'
