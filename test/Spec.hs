import System.Exit
import Sheet1.Ex3

main :: IO ()
main = do
    good <- and <$> sequence [runTests]
    if good
        then exitSuccess
        else exitFailure
