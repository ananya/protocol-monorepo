{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE TypeFamilies           #-}

module Money.Superfluid.System
    ( Address
    , Account (..)
    , balanceOfAccountAt
    , sumAccounts
    , AccountStorageInstruction (..)
    , SuperfluidToken (..)
    ) where

import           Data.Default
import           Data.Kind                                                (Type)

import           Money.Superfluid.Concepts.AccountingUnit                 (AccountingUnit (..))
import           Money.Superfluid.Concepts.Agreement
    ( AnyAgreementAccountData
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


-- | Account type class
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
    (+)
    def
    (map (flip providedBalanceOfAnyAgreement t) (agreementsOfAccount holderAccount))

sumAccounts :: Account acc => [acc] -> AU_TS acc -> AU_RTB acc
sumAccounts alist t = foldr (+) def (map (flip balanceOfAccountAt t) alist)

-- ============================================================================
-- | AccountStorageInstruction Sum Type
--
data AccountStorageInstruction acc where
    UpdateLiquidity :: Account acc
        => (ACC_ADDR acc, TBA.TBAAccountData acc) -> AccountStorageInstruction acc
    UpdateFlow :: Account acc
        => (ACC_ADDR acc, ACC_ADDR acc, CFA.CFAContractData acc) -> AccountStorageInstruction acc
    UpdateAccountFlow :: Account acc
        => (ACC_ADDR acc, CFA.CFAAccountData acc) -> AccountStorageInstruction acc

-- ============================================================================
-- | SuperfluidToken Type Class
--
-- Naming conventions:
--   * Type name: tk
--   * Term name: *Token
--
-- Notes:
--
-- * SuperfluidToken is a monadic type, where all its functions run within the monadic context.
-- * SuperfluidToken provides:
--   * addressable account,
--   * and agreement (TBA/CFA/GDA) operations.
-- * Instructions for write operations are executed in `execSFStorageInstructions`.
--
class (Monad tk , Account (TK_ACC tk)) => SuperfluidToken tk where

    type TK_ACC tk :: Type

    --
    -- System operations
    --
    getCurrentTime :: tk (AU_TS (TK_ACC tk))

    execSFStorageInstructions :: AU_TS (TK_ACC tk) -> [AccountStorageInstruction (TK_ACC tk)] -> tk ()

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
        let account' = (TBA.mintLiquidity . getTBAAccountData) account liquidity
        execSFStorageInstructions t [ UpdateLiquidity (addr, account') ]

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
                (flowACD, (getCFAAccountData senderAccount), (getCFAAccountData receiverAccount))
                newFlowRate (BBS.BufferLiquidity flowBuffer) t
        execSFStorageInstructions t
            [ UpdateFlow (senderAddr, receiverAddr, flowACD')
            , UpdateAccountFlow (senderAddr, senderFlowAAD')
            , UpdateAccountFlow (receiverAddr, receiverFlowAAD')
            ]
