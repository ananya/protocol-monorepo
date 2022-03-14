{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE TypeFamilies     #-}

module Money.Superfluid.System
    ( Address
    , Account (..)
    , balanceOfAccountAt
    , sumAccounts
    , TokenInstruction (..)
    , Token (..)
    ) where

import           Data.Default
import           Data.Kind                                                (Type)

import           Money.Superfluid.Concepts.AccountingUnit                 (AccountingUnit (..))
import           Money.Superfluid.Concepts.Liquidity                      (LiquidityVelocity (..))
--
import           Money.Superfluid.Concepts.Agreement
    ( AgreementAccountData
    , AgreementContractData
    , AnyAgreementAccountData
    , providedBalanceOfAnyAgreement
    )
--
import qualified Money.Superfluid.Agreements.ConstantFlowAgreement        as CFA
import qualified Money.Superfluid.Agreements.TransferableBalanceAgreement as TBA
--
import qualified Money.Superfluid.SubSystems.BufferBasedSolvency          as BBS


-- | Address Type Class
--
-- Naming conventions:
--  * Type name: addr
--  * Type family name: ACC_ADDR
class (Eq addr, Show addr) => Address addr


-- | AccountingUnit type class
--
-- Naming conventions:
--   * Type name: acc
--   * Type family name: SF_ACC
--   * Term name: *Account
class (AccountingUnit acc) => Account acc where

    type ACC_ADDR acc :: Type

    getTBAAccountData :: acc -> TBA.TBAAccountData acc

    getCFAAccountData :: acc -> CFA.CFAAccountData acc

    showAccountAt :: acc -> AU_TS acc -> String

    addressOfAccount :: acc -> ACC_ADDR acc

    agreementsOfAccount :: acc -> [AnyAgreementAccountData acc]

balanceOfAccountAt :: Account acc => acc -> AU_TS acc -> AU_RTB acc
balanceOfAccountAt holderAccount t = foldr
    ((+) . (`providedBalanceOfAnyAgreement` t))
    def
    (agreementsOfAccount holderAccount)

sumAccounts :: Account acc => [acc] -> AU_TS acc -> AU_RTB acc
sumAccounts alist t = foldr ((+) . (`balanceOfAccountAt` t)) def alist

-- ============================================================================
-- | AccountStorageInstruction Sum Type
--
data TokenInstruction acc where
    UpdateAgreementContractData
        :: AgreementContractData acd acc
        => [ACC_ADDR acc] -> acd -> TokenInstruction acc
    UpdateAgreementAccountData
        :: AgreementAccountData aad acc
        => ACC_ADDR acc -> aad -> TokenInstruction acc

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
class (Monad tk , Account (TK_ACC tk)) => Token tk where

    type TK_ACC tk :: Type

    --
    -- System operations
    --
    getCurrentTime :: tk (AU_TS (TK_ACC tk))

    execTokenInstructions :: AU_TS (TK_ACC tk) -> [TokenInstruction (TK_ACC tk)] -> tk ()

    --
    -- Account operations
    --
    getAccount :: ACC_ADDR (TK_ACC tk) -> tk (TK_ACC tk)

    balanceOfAccount :: ACC_ADDR (TK_ACC tk) -> tk (AU_RTB (TK_ACC tk))
    balanceOfAccount addr = do
        t <- getCurrentTime
        account <- getAccount addr
        return $ balanceOfAccountAt account t

    --
    -- TBA functions
    --
    mintLiquidity :: ACC_ADDR (TK_ACC tk) -> AU_LQ (TK_ACC tk) -> tk ()
    mintLiquidity addr liquidity = do
        t <- getCurrentTime
        account <- getAccount addr
        let tbaAAD' = (TBA.mintLiquidity . getTBAAccountData) account liquidity
        execTokenInstructions t [ UpdateAgreementAccountData addr tbaAAD' ]

    --
    -- CFA functions
    --
    getFlow :: ACC_ADDR (TK_ACC tk) -> ACC_ADDR (TK_ACC tk) -> tk (CFA.CFAContractData (TK_ACC tk))

    calcFlowBuffer :: AU_LQ (TK_ACC tk) -> tk (AU_LQ (TK_ACC tk))

    updateFlow :: ACC_ADDR (TK_ACC tk) -> ACC_ADDR (TK_ACC tk) -> AU_LQ (TK_ACC tk) -> tk ()
    updateFlow senderAddr receiverAddr newFlowRate = do
        t <- getCurrentTime
        senderAccount <- getAccount senderAddr
        receiverAccount <- getAccount receiverAddr
        flowACD <- getFlow senderAddr receiverAddr
        flowBuffer <-  calcFlowBuffer newFlowRate
        let (flowACD', senderFlowAAD', receiverFlowAAD') = CFA.updateFlow
                (flowACD, getCFAAccountData senderAccount, getCFAAccountData receiverAccount)
                (liquidityPerTimeUnit newFlowRate) (BBS.BufferLiquidity flowBuffer) t
        execTokenInstructions t
            [ UpdateAgreementContractData [senderAddr, receiverAddr] flowACD'
            , UpdateAgreementAccountData senderAddr senderFlowAAD'
            , UpdateAgreementAccountData receiverAddr receiverFlowAAD'
            ]
