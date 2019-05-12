{-# LANGUAGE TemplateHaskell #-}
module Sheet2.Ex3 where

import Test.QuickCheck.All

smallestFactor :: Int -> Int
smallestFactor n
    | n < 2 = error "There is no factor for numbers smaller 2."
    | otherwise = head $ head $ filter (elem n) products
        where
            ks = [2..n]
            ms = [1..(div n 2)]
            products = zipWith (\k ms -> map (\m -> k * m) ms) ks (repeat ms)

return []
runTests :: IO Bool
runTests = $quickCheckAll
