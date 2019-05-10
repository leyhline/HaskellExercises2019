import System.Exit
import Sheet1.Ex1 as Sheet1Ex1
import Sheet1.Ex2 as Sheet1Ex2
import Sheet1.Ex3 as Sheet1Ex3
import Sheet2.Ex1 as Sheet2Ex1
import Sheet2.Ex2 as Sheet2Ex2

main :: IO ()
main = do
    good <- and <$> sequence [
        Sheet1Ex1.runTests,
        Sheet1Ex2.runTests,
        Sheet1Ex3.runTests,
        Sheet2Ex1.runTests,
        Sheet2Ex2.runTests]
    if good
        then exitSuccess
        else exitFailure
