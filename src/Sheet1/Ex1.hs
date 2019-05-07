{-# LANGUAGE TemplateHaskell #-}
module Sheet1.Ex1 where

import Data.List
import Test.QuickCheck.All

maxi :: Int -> Int -> Int
maxi x y
    | x > y = x
    | otherwise = y

prop_CompareMaxiWithMax x y = maxi x y == max x y

mini :: Int -> Int -> Int
mini x y
    | x > y = y
    | otherwise  = x

prop_CompareMiniWithMin x y = mini x y == min x y

max3 :: Int -> Int -> Int -> Int
max3 x y z = maxi x $ maxi y z

prop_CompareMax3WithMaximum x y z = max3 x y z == maximum [x,y,z]

med :: Int -> Int -> Int -> Int
med x y z = max3 (mini x y) (mini y z) (mini x z)

prop_MiddleOfSortedList x y z = med x y z == sort [x,y,z] !! 1

return []
runTests :: IO Bool
runTests = $quickCheckAll
