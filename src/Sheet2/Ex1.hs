{-# LANGUAGE TemplateHaskell #-}
module Sheet2.Ex1 where

import Test.QuickCheck.All

fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n
    | n > 1 = fib (n-1) + fib (n-2)
    | otherwise = error "Function not defined for negative numbers."

fibFast :: Int -> Integer
fibFast n = fibList !! n 

fibList :: [Integer]
fibList = 0 : 1 : zipWith (+) fibList (tail fibList)

prop_CompareWithNaiveFib n = fib (mod n 20) == fibFast (mod n 20)

return []
runTests :: IO Bool
runTests = $quickCheckAll
