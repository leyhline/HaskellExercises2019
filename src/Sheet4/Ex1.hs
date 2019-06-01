{-# LANGUAGE TemplateHaskell #-}
module Sheet4.Ex1 where

import Test.QuickCheck.All

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ y [] = y
foldr' f y (x:xs) = f x (foldr' f y xs)

prop_CompareWithBuiltinFoldr y xs = foldr' (++) y xs == foldr (++) y xs

or' :: [Bool] -> Bool
or' = foldr' (||) False 

prop_CompareWithBuiltinOr x = or' x == or x

filter' :: (a -> Bool) -> [a] -> [a]
filter' f = foldr' (\x y -> if f x then x : y else y) []

prop_CompareWithBuiltinFilter x = filter' odd x == filter odd x

map' :: (a -> b) -> [a] -> [b]
map' f = foldr' (\x y -> f x : y) []

addOne x = x + 1
prop_CompareWithBuiltinMap x = map' addOne x == map addOne x

-- TODO: What?
-- | From <https://wiki.haskell.org/Foldl_as_foldr>
foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' f a bs = foldr' (\b g x -> g (f x b)) id bs a

prop_CompareWithBuiltinFoldl y xs = foldl' (++) y xs == foldl (++) y xs

remdups :: Eq a => [a] -> [a]
remdups = foldr' checkNextElement []
    where
        checkNextElement x [] = [x]
        checkNextElement x (y:ys) = if x == y then x : ys else x : y : ys

remdups' :: Eq a => [a] -> [a]
remdups' [] = []
remdups' [x] = [x]
remdups' (x:y:ys) = if x == y then remdups' (y:ys) else x : remdups' (y : ys)

prop_RemdupsNaiveVsFold :: [Int] -> Bool
prop_RemdupsNaiveVsFold x = remdups x == remdups' x

return []
runTests :: IO Bool
runTests = $quickCheckAll
