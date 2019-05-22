{-# LANGUAGE TemplateHaskell #-}
module Sheet2.Ex2 where

import Data.List
import Test.QuickCheck.All

undup :: Eq a => [a] -> [a]
undup x = reverse $ buildUndupList ([], x)

buildUndupList :: Eq a => ([a], [a]) -> [a]
buildUndupList (unduplicated, []) = unduplicated
buildUndupList (unduplicated, current:remaining)
    = if elem current unduplicated
        then buildUndupList (unduplicated, remaining)
        else buildUndupList (current:unduplicated, remaining)

prop_CompareWithNub :: [Int] -> Bool
prop_CompareWithNub x = undup x == nub x

return []
runTests :: IO Bool
runTests = $quickCheckAll
