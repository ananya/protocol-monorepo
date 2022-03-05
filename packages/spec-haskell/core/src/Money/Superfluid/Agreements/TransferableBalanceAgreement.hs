{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Money.Superfluid.Agreements.TransferableBalanceAgreement
    ( TBAAccountData
    , mintLiquidity
    , burnLiquidity
    , transferLiquidity
    ) where

import           Data.Default
import           Text.Printf

import           Money.Superfluid.Concepts.AccountingUnit  (AccountingUnit (..))
import           Money.Superfluid.Concepts.Agreement       (AgreementAccountData (..))
import           Money.Superfluid.Concepts.RealtimeBalance (untappedLiquidityToRTB)


-- ============================================================================
-- | TBAAccountData Type (is AgreementAccountData)
--
data AccountingUnit au => TBAAccountData au = TBAAccountData
    { settledAt :: AU_TS au
    , liquidity :: AU_LQ au
    }

instance AccountingUnit au => Default (TBAAccountData au) where
    def = TBAAccountData { settledAt = def, liquidity = def }

instance AccountingUnit au => AgreementAccountData (TBAAccountData au) au where
    providedBalanceOfAgreement a _ = untappedLiquidityToRTB $ liquidity a

instance AccountingUnit au => Show (TBAAccountData au) where
    show x = printf "{ settledAt = %s, liquidity = %s }" (show $ settledAt x) (show $ liquidity x)

-- ============================================================================
-- TBA Operations
--
mintLiquidity :: AccountingUnit au => TBAAccountData au -> AU_LQ au -> TBAAccountData au
mintLiquidity a l = a { liquidity = (liquidity a) + l }

burnLiquidity :: AccountingUnit au => TBAAccountData au -> AU_LQ au -> TBAAccountData au
burnLiquidity a l = a { liquidity = (liquidity a) - l }

transferLiquidity :: AccountingUnit au
    => (TBAAccountData au, TBAAccountData au) -> AU_LQ au
    -> (TBAAccountData au, TBAAccountData au)
transferLiquidity (from, to) l =
    ( from { liquidity = (liquidity from) - l }
    , to   { liquidity = (liquidity   to) + l })
