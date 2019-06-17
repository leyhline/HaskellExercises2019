module Sheet5.Ex1 where

import System.IO
import Data.Char
import qualified Data.Set as Set

initialGuess = 50 :: Int
maxNumber = 100 :: Int
greaterSynonyms = Set.fromList ["greater", "g", "larger", "bigger", ">"]
smallerSynonyms = Set.fromList ["smaller", "s", "lesser", "<"]
yesSynonyms = Set.fromList ["yes", "y", "ok", "okay", "right", "correct", "="]

initialInstruction :: IO ()
initialInstruction = do
    putStrLn ("Choose a number between 1 and " ++ show maxNumber ++ "!")
    putStrLn "Then tell me if it is smaller or greater than my guess or ‘Yes’ if I guessed right."

askForNumber :: Int -> IO ()
askForNumber n = putStrLn ("Is it " ++ show n ++ "?")

readLowercaseLine :: IO String
readLowercaseLine = putStr "> " >> hFlush stdout >> getLine >>= \input -> return $ map toLower input

parseResponse :: String -> Maybe Ordering
parseResponse s
    | Set.member s greaterSynonyms = Just GT
    | Set.member s smallerSynonyms = Just LT
    | Set.member s yesSynonyms = Just EQ
    | otherwise = Nothing

getResponse :: Int -> IO (Maybe Ordering)
getResponse n = askForNumber n >> parseResponse <$> readLowercaseLine
    
questionLoop :: Maybe Ordering -> Int -> Int -> Int -> IO ()
questionLoop ord attempt number lastInterval
    | attempt > 10 = error "Are you sure about your number? I give up!"
    | ord == Just EQ = putStrLn ("I won in " ++ show attempt ++ " attempts!")
    | ord == Nothing = getResponse number >>= \response -> questionLoop response attempt number lastInterval
    | ord == Just GT = getResponse (number+newInterval) >>= \response -> questionLoop response (attempt+1) (number+newInterval) newInterval
    | ord == Just LT = getResponse (number-newInterval) >>= \response -> questionLoop response (attempt+1) (number-newInterval) newInterval
    where newInterval = (lastInterval `div` 2) + (lastInterval `mod` 2)

mainLoop :: IO ()
mainLoop = initialInstruction >> questionLoop Nothing 1 50 50
