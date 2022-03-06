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
import           Money.Superfluid.Concepts.Liquidity       (UntappedLiquidity (..), untypeLiquidity)
import           Money.Superfluid.Concepts.RealtimeBalance (untappedLiquidityToRTB)
--
import           Money.Superfluid.Concepts.Agreement       (AgreementAccountData (..))



-- ============================================================================
-- | TBAAccountData Type (is AgreementAccountData)
--
data AccountingUnit au => TBAAccountData au = TBAAccountData
    { settledAt :: AU_TS au
    , liquidity :: UntappedLiquidity (AU_LQ au)
    }

_untypedLiquidity :: AccountingUnit au => TBAAccountData au -> AU_LQ au
_untypedLiquidity = untypeLiquidity . liquidity

instance AccountingUnit au => Default (TBAAccountData au) where
    def = TBAAccountData { settledAt = def, liquidity = UntappedLiquidity def }

instance AccountingUnit au => AgreementAccountData (TBAAccountData au) au where
    providedBalanceOfAgreement a _ = untappedLiquidityToRTB $ _untypedLiquidity a

instance AccountingUnit au => Show (TBAAccountData au) where
    show x = printf "{ t = %s, uliq = %s }" (show $ settledAt x) (show $ _untypedLiquidity x)

-- ============================================================================
-- TBA Operations
--
mintLiquidity :: AccountingUnit au => TBAAccountData au -> AU_LQ au -> TBAAccountData au
mintLiquidity a l = a { liquidity = UntappedLiquidity $ (_untypedLiquidity a) + l }

burnLiquidity :: AccountingUnit au => TBAAccountData au -> AU_LQ au -> TBAAccountData au
burnLiquidity a l = a { liquidity = UntappedLiquidity $ (_untypedLiquidity a) - l }

transferLiquidity :: AccountingUnit au
    => (TBAAccountData au, TBAAccountData au) -> AU_LQ au
    -> (TBAAccountData au, TBAAccountData au)
transferLiquidity (from, to) l =
    ( from { liquidity = UntappedLiquidity $ (_untypedLiquidity from) - l }
    , to   { liquidity = UntappedLiquidity $ (_untypedLiquidity   to) + l })
