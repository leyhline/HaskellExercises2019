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

smallestFactor' :: Int -> Int
smallestFactor' n
    | n < 2 = error "There is no factor for numbers smaller 2."
    | otherwise = minimum ks
        where ks = filter (>= 2) $ map fst $ filter (\x -> snd x == 0) $ map (\m -> (div n m, mod n m)) [1..n]

prop_CompareSmallestFactorImplementations n = smallestFactor n' == smallestFactor' n'
    where n' = abs n + 2

return []
runTests :: IO Bool
runTests = $quickCheckAll
