{-# LANGUAGE TemplateHaskell #-}
module Sheet2.Ex1 where

import Test.QuickCheck
import Test.QuickCheck.All

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n
    | n > 1 = fib (n-1) + fib (n-2)
    | otherwise = error "Function not defined for negative numbers."

fibFast :: Integer -> Integer
fibFast n = fibList !! fromInteger n

fibList :: [Integer]
fibList = 0 : 1 : zipWith (+) fibList (tail fibList)

boundedInt :: Integer -> Gen Integer
boundedInt n = suchThat arbitrary (\x -> x > 0 && x < n)

prop_CompareWithNaiveFib = forAll (boundedInt 20) (\n -> fib n == fibFast n)

return []
runTests :: IO Bool
runTests = $quickCheckAll
