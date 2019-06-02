import System.Exit
import qualified Sheet1.Ex1(runTests)
import qualified Sheet1.Ex2(runTests)
import qualified Sheet1.Ex3(runTests)
import qualified Sheet2.Ex1(runTests)
import qualified Sheet2.Ex2(runTests)
import qualified Sheet2.Ex3(runTests)
import qualified Sheet4.Ex1(runTests)
import qualified Sheet4.Ex2(runTests)
import qualified Sheet4.Ex3(runTests)
import qualified Sheet4.Ex4(runTests)

main :: IO ()
main = do
    good <- and <$> sequence
        [Sheet1.Ex1.runTests
        ,Sheet1.Ex2.runTests
        ,Sheet1.Ex3.runTests
        ,Sheet2.Ex1.runTests
        ,Sheet2.Ex2.runTests
        ,Sheet2.Ex3.runTests
        ,Sheet4.Ex1.runTests
        ,Sheet4.Ex2.runTests
        ,Sheet4.Ex3.runTests
        ,Sheet4.Ex4.runTests]
    if good
        then exitSuccess
        else exitFailure
