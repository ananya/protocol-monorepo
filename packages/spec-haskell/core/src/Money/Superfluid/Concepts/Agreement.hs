{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Money.Superfluid.Concepts.Agreement
    ( AgreementContractData
    , AgreementAccountData (..)
    , AnyAgreementAccountData (MkAgreementAccountData)
    , providedBalanceOfAnyAgreement
    ) where

import           Data.Default

import           Money.Superfluid.Concepts.AccountingUnit (AccountingUnit (..))


-- ============================================================================
-- | AgreementContractData type class
--
-- Naming conventions:
--  * Type name: acd
class (Default acd, Show acd, AccountingUnit au)
    => AgreementContractData acd au | acd -> au where

-- ============================================================================
-- | AgreementAccountData type class
--
-- Naming conventions:
--  - Type name: aad
class (Default aad, Show aad, AccountingUnit au)
    => AgreementAccountData aad au | aad -> au where

    providedBalanceOfAgreement :: aad -> AU_TS au -> AU_RTB au

-- ============================================================================
-- | AnyAgreementAccountData type
--
-- Naming conventions:
--  - Type name: aaad
--  - Term name: anyAgreement
--
-- Notes:
-- - To Enumerate all supported agreements using GADTs
--   See: https://wiki.haskell.org/Heterogenous_collections
-- - MkAgreementAccountData is the constructor
-- - providedBalanceOfAnyAgreement is convenience wrapper of providedBalanceOfAgreement
data AnyAgreementAccountData au where
    MkAgreementAccountData
        :: (AccountingUnit au, AgreementAccountData aad au)
        => aad -> AnyAgreementAccountData au

-- | providedBalanceOfAgreement wrapper for AnyAgreementAccountData
providedBalanceOfAnyAgreement
    :: AccountingUnit au
    => AnyAgreementAccountData au -> AU_TS au -> AU_RTB au
providedBalanceOfAnyAgreement (MkAgreementAccountData g) = providedBalanceOfAgreement g
