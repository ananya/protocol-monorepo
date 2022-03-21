import           System.Exit
import           Test.HUnit

import qualified Money.Superfluid.System_Test

main :: IO ()
main = do
    results <- runTestTT Money.Superfluid.System_Test.tests
    if errors results + failures results == 0
    then
        exitSuccess
    else
        exitWith (ExitFailure 1)
