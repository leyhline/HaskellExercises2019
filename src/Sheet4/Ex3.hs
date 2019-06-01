{-# LANGUAGE TemplateHaskell #-}
module Sheet4.Ex3 where

import Data.List(sort)
import Test.QuickCheck.All

mergeBy :: Ord a => [a] -> [a] -> [a]
mergeBy [] ys = ys
mergeBy xs [] = xs
mergeBy (x:xs) (y:ys) = if x < y
    then x : mergeBy xs (y:ys)
    else y : mergeBy (x:xs) ys

prop_MergeByReturnsSortedList :: [Int] -> [Int] -> Bool
prop_MergeByReturnsSortedList xs ys = let sortedList = mergeBy (sort xs) (sort ys) in
    sortedList == sort sortedList

-- TODO: Think about this some more, best with just pen and paper.
-- |From <https://rosettacode.org/wiki/hamming_numbers#Haskell>
hammingPlag :: [Integer]
hammingPlag = 1 : map (2*) hammingPlag `union` map (3*) hammingPlag `union` map (5*) hammingPlag
union a@(x:xs) b@(y:ys) = case compare x y of
    LT -> x : union xs b
    EQ -> x : union xs ys
    GT -> y : union a  ys

return []
runTests :: IO Bool
runTests = $quickCheckAll
