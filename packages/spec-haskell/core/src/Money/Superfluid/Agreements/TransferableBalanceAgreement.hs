{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Money.Superfluid.Agreements.TransferableBalanceAgreement
    ( TBAAccountData (..)
    , mintLiquidity
    , burnLiquidity
    , transferLiquidity
    ) where

import           Data.Default
import           Text.Printf

import           Money.Superfluid.Concepts.Agreement       (AgreementAccountData (..))
import           Money.Superfluid.Concepts.Liquidity       (UntappedLiquidity (..), untypeLiquidity)
import           Money.Superfluid.Concepts.RealtimeBalance (untappedLiquidityToRTB)
import           Money.Superfluid.Concepts.SuperfluidTypes (SuperfluidTypes (..))
import           Money.Superfluid.Concepts.TaggedTypeable



-- ============================================================================
-- | TBAAccountData Type (is AgreementAccountData)
--
data SuperfluidTypes sft => TBAAccountData sft = TBAAccountData
    { settledAt :: SFT_TS sft
    , liquidity :: UntappedLiquidity (SFT_LQ sft)
    }
instance SuperfluidTypes sft => TaggedTypeable (TBAAccountData sft) where proxyTag _ = "TBA"
instance SuperfluidTypes sft => Default (TBAAccountData sft) where
    def = TBAAccountData { settledAt = def, liquidity = def }

_untypedLiquidity :: SuperfluidTypes sft => TBAAccountData sft -> SFT_LQ sft
_untypedLiquidity = untypeLiquidity . liquidity
--
-- instance SuperfluidTypes sft => Default (TBAAccountData sft) where
--     def = TBAAccountData { settledAt = def, liquidity = UntappedLiquidity def }

instance SuperfluidTypes sft => AgreementAccountData (TBAAccountData sft) sft where
    providedBalanceOfAgreement a _ = untappedLiquidityToRTB $ _untypedLiquidity a

instance SuperfluidTypes sft => Show (TBAAccountData sft) where
    show x = printf "{ t = %s, uliq = %s }" (show $ settledAt x) (show $ _untypedLiquidity x)

-- ============================================================================
-- TBA Operations
--
mintLiquidity :: SuperfluidTypes sft => TBAAccountData sft -> SFT_LQ sft -> TBAAccountData sft
mintLiquidity a l = a { liquidity = UntappedLiquidity $ _untypedLiquidity a + l }

burnLiquidity :: SuperfluidTypes sft => TBAAccountData sft -> SFT_LQ sft -> TBAAccountData sft
burnLiquidity a l = a { liquidity = UntappedLiquidity $ _untypedLiquidity a - l }

transferLiquidity :: SuperfluidTypes sft
    => (TBAAccountData sft, TBAAccountData sft) -> SFT_LQ sft
    -> (TBAAccountData sft, TBAAccountData sft)
transferLiquidity (from, to) l =
    ( from { liquidity = UntappedLiquidity $ _untypedLiquidity from - l }
    , to   { liquidity = UntappedLiquidity $ _untypedLiquidity   to + l })
