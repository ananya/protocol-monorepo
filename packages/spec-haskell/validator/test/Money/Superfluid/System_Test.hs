module Money.Superfluid.System_Test where

import           Control.Monad.IO.Class
import           Test.HUnit

import qualified Money.Superfluid.Instances.Simple.System as SF
--
import           Money.Superfluid.TokenTester


simple1to1ScenarioTest :: TokenTestCase
simple1to1ScenarioTest = TokenTestCase TokenTestSpec
    { testLabel = "Simple 1to1 Scenario Test"
    , testAddressesToInit = ["alice", "bob", "carol"]
    , testAccountInitBalance = constInitBalance
    } (\ctx -> do
    let [alice, bob, carol] = testAddresses ctx
    -- T0: test initial condition
    expeceTotalBalanceTo "total balance stays the same" (== 3 * constInitBalance)
    accounts' <- runToken SF.listAccounts
    liftIO $ assertEqual "expected number of accounts" 3 (length accounts')

    -- T1: test initial condition
    -- creating flow: alice -> bob @ 0.0001/s
    runToken $ SF.updateFlow alice bob (SF.toWad (0.0001 :: Double))
    expectCFANetFlowRateTo "alice should have -1x net flowrate" alice (== SF.toWad(-0.0001 :: Double))
    expectCFANetFlowRateTo "alice should have 1x net flowrate" bob (== SF.toWad(0.0001 :: Double))
    expectCFANetFlowRateTo "alice should have zero net flowrate" carol (== SF.toWad(0.0000 :: Double))

    -- T2: move time forward and test balance moves
    timeTravel $ 3600 * 24
    expectAccountBalanceTo "alice should send money" alice (< constInitBalance)
    expectAccountBalanceTo "bob should receive money" bob (> constInitBalance)
    expectAccountBalanceTo "carol should be the same" carol (== constInitBalance)
    expeceTotalBalanceTo "total balance stays the same" (== 3 * constInitBalance)
    )

simple1to2ScenarioTest :: TokenTestCase
simple1to2ScenarioTest = TokenTestCase TokenTestSpec
    { testLabel = "Simple 1to2 Scenario Test"
    , testAddressesToInit = ["alice", "bob", "carol"]
    , testAccountInitBalance = constInitBalance
    } (\ctx -> do
    -- T0: test initial condition
    let [alice, bob, carol] = testAddresses ctx
    runToken $ SF.updateFlow alice bob (SF.toWad (0.0001 :: Double))
    runToken $ SF.updateFlow alice carol (SF.toWad (0.0001 :: Double))
    expectCFANetFlowRateTo "alice should have -2x net flowrate" alice (== SF.toWad(-0.0002 :: Double))
    expectCFANetFlowRateTo "alice should have 1x net flowrate" bob (== SF.toWad(0.0001 :: Double))
    expectCFANetFlowRateTo "alice should have 1x net flowrate" carol (== SF.toWad(0.0001 :: Double))

    -- T1: move time forward and test balance moves
    timeTravel $ 3600 * 24
    expectAccountBalanceTo "alice should send money" alice (< constInitBalance)
    expectAccountBalanceTo "bob should receive money" bob (> constInitBalance)
    expectAccountBalanceTo "carol should also receive money" carol (> constInitBalance)
    expeceTotalBalanceTo "total balance stays the same" (== 3 * constInitBalance)
    )

tests :: Test
tests = TestList $ map createTokenTestCase
    [ simple1to1ScenarioTest
    , simple1to2ScenarioTest
    ]
