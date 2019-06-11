module Sheet5.Ex1 where

import System.IO
import Data.Char

initialGuess = 50 :: Int
maxNumber = 100 :: Int

initialInstruction :: IO ()
initialInstruction = do
    putStrLn ("Choose a number between 1 and " ++ show maxNumber ++ "!")
    putStrLn "Then tell me if it is smaller or greater than my guess or ‘Yes’ if I guessed right."

askForNumber :: Int -> IO ()
askForNumber n = putStrLn ("Is it " ++ show n ++ "?")

readLowercaseLine :: IO String
readLowercaseLine = putStr "> " >> hFlush stdout >> getLine >>= \input -> return $ map toLower input

parseResponse :: String -> Maybe Ordering
parseResponse "greater" = Just GT
parseResponse "smaller" = Just LT
parseResponse "yes" = Just EQ
parseResponse _ = Nothing

getResponse :: Int -> IO (Maybe Ordering)
getResponse n = askForNumber n >> parseResponse <$> readLowercaseLine
    
questionLoop :: Maybe Ordering -> Int -> Int -> IO ()
questionLoop (Just EQ) attempt _ = putStrLn ("I won in " ++ show attempt ++ " attempts!")
questionLoop (Just GT) attempt number = getResponse (number + (number `div` 2)) >>= \response -> questionLoop response (attempt+1) (number + (number `div` 2))
questionLoop (Just LT) attempt number = getResponse (number - (number `div` 2)) >>= \response -> questionLoop response (attempt+1) (number - (number `div` 2))
questionLoop Nothing attempt number = getResponse number >>= \response -> questionLoop response attempt number

mainLoop :: IO ()
mainLoop = initialInstruction >> questionLoop Nothing 1 50
