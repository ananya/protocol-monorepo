module Money.Superfluid.TokenTester where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.Default
import           Data.Maybe
import           GHC.Stack
import           Test.HUnit

import qualified Money.Superfluid.Concepts.RealtimeBalance         as RTB
--
import qualified Money.Superfluid.Agreements.ConstantFlowAgreement as CFA

import qualified Money.Superfluid.Instances.Simple.System          as SF


constInitBalance :: SF.Wad
constInitBalance = SF.toWad (100.0 :: Double)

cZeroWad :: SF.Wad
cZeroWad = SF.toWad (0 :: Double)

-- ============================================================================
-- | TokenTester TestCase Creator
--
type TokenMonad = SF.SimpleTokenStateT IO

data TokenTesterData = TokenTesterData
    { sfSys :: SF.SimpleSystemData
    , token :: SF.SimpleTokenData
    }
type TokenTester = StateT TokenTesterData IO

data TokenTestSpec = TokenTestSpec
    { testLabel              :: String
    , testAddressesToInit    :: [String]
    , testAccountInitBalance :: SF.Wad
    }

data TokenTestContext = TokenTestContext
    { testAddresses :: [SF.SimpleAddress]
    }

data TokenTestCase = TokenTestCase TokenTestSpec (TokenTestContext -> TokenTester ())

createTokenTestCase :: TokenTestCase -> Test
createTokenTestCase (TokenTestCase spec runner) = TestLabel (testLabel spec) $ TestCase $ do
    evalStateT (do
        let addresses = map (fromJust . SF.createSimpleAddress) (testAddressesToInit spec)
        mapM_ (`createTestAccount` testAccountInitBalance spec) addresses
        runner TokenTestContext { testAddresses = addresses }
        ) TokenTesterData
        { sfSys = SF.SimpleSystemData { SF.currentTime = 0 }
        , token = def
        }

-- ============================================================================
-- | TokenTester Operations
--
timeTravel :: HasCallStack => SF.SimpleTimestamp -> TokenTester ()
timeTravel d = modify (\vs -> vs { sfSys = (sfSys vs) { SF.currentTime = (+ d) . SF.currentTime . sfSys $ vs } })

runToken :: HasCallStack => TokenMonad a -> TokenTester a
runToken m = do
    s <- get
    (a, token') <- liftIO $ SF.runSimpleTokenStateT m (sfSys s) (token s)
    modify (\vs -> vs { token = token' })
    return a

createTestAccount :: HasCallStack => SF.SimpleAddress -> SF.Wad -> TokenTester ()
createTestAccount addr initBalance = runToken $ do
    SF.mintLiquidity addr initBalance
    acc <- SF.getAccount addr
    SF.addAccount addr acc

-- ============================================================================
-- | TokenTester Assertions
--
expectAccountBalanceTo :: HasCallStack => String -> SF.SimpleAddress -> (SF.Wad -> Bool) -> TokenTester ()
expectAccountBalanceTo label addr expr = do
    balance <- runToken $ SF.balanceOfAccount addr
    liftIO $ assertBool label (expr . RTB.liquidityRequiredForRTB $ balance)

expeceTotalBalanceTo :: HasCallStack => String -> (SF.Wad -> Bool) -> TokenTester ()
expeceTotalBalanceTo label expr = do
    t <- runToken SF.getCurrentTime
    accounts <- runToken SF.listAccounts
    liftIO $ assertBool label (expr . RTB.liquidityRequiredForRTB $ SF.sumAccounts (map snd accounts) t)

expectCFANetFlowRateTo :: HasCallStack => String -> SF.SimpleAddress -> (SF.Wad -> Bool) -> TokenTester ()
expectCFANetFlowRateTo label addr expr = do
    account <- runToken $ SF.getAccount addr
    liftIO $ assertBool label (expr . SF.liquidityTimesTimeUnit . CFA.netFlowRate . SF.accountCFA $ account)
