{-# LANGUAGE TemplateHaskell #-}
module Sheet1.Ex2 where

import Data.Char
import Test.QuickCheck.All

initialStack = [0,0..] :: [Int]

push :: Int -> [Int] -> [Int]
push n stack = n:stack

prop_PushToFirstIndex n stack = head (push n stack) == n

pop :: [Int] -> [Int]
pop = tail

dup :: [Int] -> [Int]
dup (x:xs) = x:x:xs

prop_CheckFirstTwoElements stack = newStack !! 0 == newStack !! 1
    where newStack = dup (stack ++ initialStack)

add :: [Int] -> [Int]
add (x:y:xs) = x+y:xs

prop_CheckFirstSumElement stack = head (add infstack) == infstack !! 0 + infstack !! 1
    where infstack = stack ++ initialStack

substract :: [Int] -> [Int]
substract (x:y:xs) = x-y:xs

prop_CheckFirstDiffElement stack = head (substract infstack) == infstack !! 0 - infstack !! 1
    where infstack = stack ++ initialStack

multiply :: [Int] -> [Int]
multiply (x:y:xs) = x*y:xs

prop_CheckFirstProductElement stack = head (multiply infstack) == infstack !! 0 * infstack !! 1
    where infstack = stack ++ initialStack

neg :: [Int] -> [Int]
neg (x:xs) = (-x):xs

prop_CheckFirstElementNegated stack = head (neg infstack) == head infstack * (-1)
    where infstack = stack ++ initialStack

readCommand :: String -> [Int] -> [Int]
readCommand commands stack = let commandList = lines commands
    in snd $ processCommandList (commandList, stack)

processCommandList :: ([String], [Int]) -> ([String], [Int])
processCommandList ([], stack) = ([], stack)
processCommandList (command:commands, stack) = processCommandList (commands, processCommand command stack)
    
processCommand :: String -> [Int] -> [Int]
processCommand "pop" = pop
processCommand "dup" = dup
processCommand "add" = add
processCommand "substract" = substract
processCommand "multiply" = multiply
processCommand "neg" = neg
processCommand other = let wordList = words other in
    if length wordList == 2 && head wordList == "push" && isDigits (wordList !! 1)
        then push (read (wordList !! 1)::Int)
        else id

isDigits :: String -> Bool
isDigits = all isDigit

prop_RandomInputIsNoop commands stack = readCommand commands stack == stack

repl :: [Int] -> IO()
repl stack = do
    putStr "> "
    command <- getLine
    stack <- return (processCommand command stack)
    print $ take 10 stack
    repl stack

return []
runTests :: IO Bool
runTests = $quickCheckAll
