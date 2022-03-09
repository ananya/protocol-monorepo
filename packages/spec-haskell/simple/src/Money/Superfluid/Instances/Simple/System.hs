{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE TypeFamilies               #-}

module Money.Superfluid.Instances.Simple.System
    ( module Money.Superfluid.Instances.Simple.SuperfluidTypes
    -- SimpleAddress
    , SimpleAddress
    , createSimpleAddress
    -- SimpleAccount
    , SimpleAccount
    , SF.Account (..)
    , SF.balanceOfAccountAt
    , SF.sumAccounts
    , listAccounts
    , addAccount
    -- SuperfluidToken
    , SimpleSystemData (..)
    , SimpleTokenData
    , SimpleTokenStateT
    , runSimpleTokenStateT
    , evalSimpleTokenStateT
    , execSimpleTokenStateT
    , getSimpleTokenData
    , SF.SuperfluidToken (..)
    , initSimpleToken)
    where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import qualified Data.Binary                                              as B
import           Data.Binary.Put                                          (runPut)
import           Data.Char
import           Data.Default
import           Data.Functor
import qualified Data.Map                                                 as M
import           Data.Maybe

import           Money.Superfluid.Concepts.AccountingUnit                 (AccountingUnit (..))
import           Money.Superfluid.Concepts.Agreement
    ( AnyAgreementAccountData (MkAgreementAccountData)
    )
--
import qualified Money.Superfluid.Agreements.ConstantFlowAgreement        as CFA
import qualified Money.Superfluid.Agreements.TransferableBalanceAgreement as TBA
--
import qualified Money.Superfluid.System                                  as SF

import           Money.Superfluid.Instances.Simple.SuperfluidTypes


-- ============================================================================
-- SimpleAddress Base Type
--
-- Note: It must consist of only alphabetical letters
--
newtype SimpleAddress = SimpleAddress String
    deriving newtype (Eq, Ord, B.Binary, SF.Address)

instance Show SimpleAddress where
    show (SimpleAddress a) = a

-- SimpleAddress public constructor
createSimpleAddress :: String -> Maybe SimpleAddress
createSimpleAddress a = if all isAlpha a then Just $ SimpleAddress a else Nothing

-- ============================================================================
-- Simple Types for Agreements
--
type SimpleTBAAccountData = TBA.TBAAccountData SimpleAccount
type SimpleCFAContractData = CFA.CFAContractData SimpleAccount
type SimpleCFAAccountData = CFA.CFAAccountData SimpleAccount

-- ============================================================================
-- SimpleAccount Type and Operations (is SuperfluidAccount)
--
data SimpleAccount = SimpleAccount
    { address       :: SimpleAddress
    , tba           :: SimpleTBAAccountData
    , cfa           :: SimpleCFAAccountData
    , lastUpdatedAt :: SimpleTimestamp
    }

instance AccountingUnit SimpleAccount where
    type AU_LQ SimpleAccount = Wad
    type AU_TS SimpleAccount = SimpleTimestamp
    type AU_LQV SimpleAccount = SimpleWadRate
    type AU_RTB SimpleAccount = SimpleRealtimeBalance

instance SF.Account SimpleAccount where
    type ACC_ADDR SimpleAccount = SimpleAddress

    getTBAAccountData = tba

    getCFAAccountData = cfa

    showAccountAt a t =
        "Account @" ++ show(address a) ++
        "\n  Balance: " ++ show(SF.balanceOfAccountAt a t :: SimpleRealtimeBalance) ++
        "\n  TBA: " ++ show(tba a) ++
        "\n  CFA: " ++ show(cfa a) ++
        "\n  Last Update: " ++ show(lastUpdatedAt a)

    addressOfAccount = address

    agreementsOfAccount a =
        [ MkAgreementAccountData $ tba a
        , MkAgreementAccountData $ cfa a
        ]

_createSimpleAccount :: SimpleAddress -> SimpleTimestamp -> SimpleAccount
_createSimpleAccount toAddress t = SimpleAccount
    { address = toAddress
    , lastUpdatedAt = t
    , tba = def
    , cfa = def
    }

_updateTBAAccountData :: SimpleAccount -> SimpleTimestamp -> SimpleTBAAccountData -> SimpleAccount
_updateTBAAccountData acc t' tba' = acc { tba = tba', lastUpdatedAt = t' }

_updateCFAAccountData :: SimpleAccount -> SimpleTimestamp -> SimpleCFAAccountData -> SimpleAccount
_updateCFAAccountData acc t' cfa' = acc { cfa = cfa', lastUpdatedAt = t' }

-- ============================================================================
-- | SimpleSystemData Type
--
newtype SimpleSystemData = SimpleSystemData
    { currentTime   :: SimpleTimestamp
    }

-- ============================================================================
-- | SimpleTokenData Type
--
data SimpleTokenData = SimpleTokenData
    { accounts      :: M.Map SimpleAddress SimpleAccount
    , cfaAgreements :: M.Map String SimpleCFAContractData
    }
instance Default SimpleTokenData where
    def = SimpleTokenData { accounts = def, cfaAgreements = def }

-- ============================================================================
-- | Simple Monad Transformer stack
newtype SimpleSystemStateT m a = SimpleSystemStateT (ReaderT SimpleSystemData m a)
    deriving newtype (Functor, Applicative, Monad, MonadTrans, MonadIO)
newtype SimpleTokenStateT m a = SimpleTokenStateT
    ( StateT SimpleTokenData
    ( SimpleSystemStateT m )
    a )
    deriving newtype (Functor, Applicative, Monad, MonadIO)
instance MonadTrans SimpleTokenStateT where
    lift m = SimpleTokenStateT $ StateT $ \s -> lift m >>= \a -> return (a, s)

getSystemData :: (Monad m) => SimpleTokenStateT m SimpleSystemData
getSystemData = SimpleTokenStateT . lift . SimpleSystemStateT $ ask

getSimpleTokenData :: (Monad m) => SimpleTokenStateT m SimpleTokenData
getSimpleTokenData = SimpleTokenStateT get

runSimpleTokenStateT :: (Monad m)
    => SimpleTokenStateT m a -> SimpleSystemData -> SimpleTokenData -> m (a, SimpleTokenData)
runSimpleTokenStateT (SimpleTokenStateT m) sys token = m'' where
        (SimpleSystemStateT m') = runStateT m token
        m'' = runReaderT m' sys

evalSimpleTokenStateT :: (Monad m)
    => SimpleTokenStateT m a -> SimpleSystemData -> SimpleTokenData -> m a
evalSimpleTokenStateT m sys token = runSimpleTokenStateT m sys token <&> fst

execSimpleTokenStateT :: (Monad m)
    => SimpleTokenStateT m a -> SimpleSystemData -> SimpleTokenData -> m SimpleTokenData
execSimpleTokenStateT m sys token = runSimpleTokenStateT m sys token <&> snd

-- | SimpleTokenStateT State Internal Operations
--
_putSimpleTokenData :: (Monad m) => SimpleTokenData -> SimpleTokenStateT m ()
_putSimpleTokenData = SimpleTokenStateT . put

_modifySimpleTokenData :: (Monad m) => (SimpleTokenData -> SimpleTokenData) -> SimpleTokenStateT m ()
_modifySimpleTokenData = SimpleTokenStateT . modify

-- | SimpleTokenStateT m is a SuperfluidToken instance
--
instance (Monad m) => SF.SuperfluidToken (SimpleTokenStateT m) where

    type TK_ACC (SimpleTokenStateT m) = SimpleAccount

    getCurrentTime = getSystemData <&> currentTime

    execSFStorageInstructions t = mapM_ (\case
        SF.UpdateLiquidity (addr, tbaLiquidity) -> do
            account <- SF.getAccount addr
            _modifySimpleTokenData (\vs -> vs {
                accounts = M.insert
                    addr
                    (_updateTBAAccountData account t tbaLiquidity)
                    (accounts vs)
            })
        SF.UpdateFlow (sender, receiver, flow) -> do
            _modifySimpleTokenData (\vs -> vs {
                cfaAgreements = M.insert
                    (show sender ++ ":" ++ show receiver)
                    flow
                    (cfaAgreements vs)
            })
        SF.UpdateAccountFlow (addr, accountFlow) -> do
            account <- SF.getAccount addr
            _modifySimpleTokenData (\vs -> vs {
                accounts = M.insert
                    addr
                    (_updateCFAAccountData account t accountFlow)
                    (accounts vs)
            })
        )

    getAccount a = getSimpleTokenData >>= \s -> return $
        fromMaybe (_createSimpleAccount a 0) $ M.lookup a (accounts s)

    calcFlowBuffer = return  . (* Wad 3600)

    getFlow a b = getSimpleTokenData >>= \s -> return $
        fromMaybe def $ M.lookup (show a ++ ":" ++ show b) (cfaAgreements s)


-- | Other SimpleTokenStateT Operations
--
initSimpleToken :: (Monad m) => [SimpleAddress] -> Wad -> SimpleTokenStateT m ()
initSimpleToken alist initBalance = do
    t <- SF.getCurrentTime
    _putSimpleTokenData SimpleTokenData
        { accounts = M.fromList $ map (\a -> (a, _createSimpleAccount a t)) alist
        , cfaAgreements = M.empty
        }
    mapM_ (`SF.mintLiquidity` initBalance) alist

addAccount :: (Monad m) => SimpleAddress -> SimpleAccount -> SimpleTokenStateT m ()
addAccount accountAddr account = _modifySimpleTokenData (\vs -> vs {
    accounts = M.insert
        accountAddr
        account
        (accounts vs)
    })

listAccounts :: (Monad m) => SimpleTokenStateT m [(SimpleAddress, SimpleAccount)]
listAccounts = getSimpleTokenData <&> M.toList . accounts
