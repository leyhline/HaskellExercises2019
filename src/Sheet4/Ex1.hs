{-# LANGUAGE TemplateHaskell #-}
module Sheet4.Ex1 where

import Test.QuickCheck.All

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ y [] = y
foldr' f y (x:xs) = foldr' f (f x y) xs

prop_CompareWithBuiltinFoldr :: Int -> [Int] -> Bool
prop_CompareWithBuiltinFoldr y xs = foldr' (+) y xs == foldr (+) y xs

or' :: [Bool] -> Bool
or' = undefined

filter' :: (a -> Bool) -> [a] -> [a]
filter' = undefined

map' :: (a -> b) -> [a] -> [b]
map' = undefined

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' = undefined

remdups :: Eq a => [a] -> [a]
remdups = undefined

return []
runTests :: IO Bool
runTests = $quickCheckAll
