module Superfluid.Concepts.BaseTypes where

import           Data.Default

-- | Liquidity Type Class
--
-- Naming conventions:
--  * Type name: lq
--  * Type family name: AU_LQ
class (Default lq, Num lq, Ord lq, Show lq) => Liquidity lq where
    integralToLiquidity :: (Integral int) => int -> lq
    integralToLiquidity = fromInteger . toInteger

-- | Timestamp Type Class
--
-- Naming conventions:
--  * Type name: ts
--  * Type family name: AU_TS
class (Default ts, Integral ts, Ord ts, Show ts) => Timestamp ts
