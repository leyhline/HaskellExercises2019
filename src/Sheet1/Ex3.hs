{-# LANGUAGE TemplateHaskell #-}
module Sheet1.Ex3 where

import Data.List
import Test.QuickCheck.All

addOne :: Int -> Int
addOne x = x + 1

smallerTen :: Int -> Bool
smallerTen x = x < 10

head' :: [a] -> a
head' (x:xs) = x

prop_LikeBuiltinHead x = head' (x++[0]) == head (x++[0])

tail' :: [a] -> [a]
tail' (x:xs) = xs

prop_LikeBuiltinTail x = tail' (x++[0]) == tail (x++[0])

init' :: [a] -> [a]
init' (x:xs) = if null xs
    then []
    else x : init' xs

prop_LikeBuiltinInit x = init' (x++[0]) == init (x++[0])

last' :: [a] -> a
last' [x] = x
last' (x:xs) = last' xs

prop_LikeBuiltinLast x = last' (x++[0]) == last (x++[0])

length' :: [a] -> Int
length' [] = 0
length' (x:xs) = 1 + length' xs

prop_LikeBuiltinLength x = length' x == length x

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs +++ [x]

prop_LikeBuiltinReverse x = reverse' x == reverse x

(+++) :: [a] -> [a] -> [a]
(+++) [] y = y
(+++) (x:xs) y = x : xs +++ y

prop_LikeBuiltinAppend x y = (x +++ y) == (x ++ y)

iterate'' :: (a -> a) -> a -> [a]
iterate'' f x = x : iterate'' f (f x)

prop_LikeBuiltinIterate x = take 10 (iterate'' addOne x) == take 10 (iterate addOne x)

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs

prop_LikeBuiltinMap x = map' addOne x == map addOne x

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs) = if f x
    then x : filter' f xs
    else filter' f xs

prop_LikeBuiltinFilter x = filter' even x == filter even x

intersperse' :: a -> [a] -> [a]
intersperse' elem [] = []
intersperse' elem [x] = [x]
intersperse' elem (x:y:xs) = x : elem : intersperse' elem (y:xs) 

prop_LikeBuiltinIntersperse elem list = intersperse' elem list == intersperse elem list

concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x +++ concat' xs

prop_LikeBuiltinConcat x = concat' x == concat x

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] y = []
zipWith' f x [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith f xs ys

prop_LikeBuiltinZipWith x y = zipWith' (+) x y == zipWith (+) x y

repeat' :: a -> [a]
repeat' x = x : repeat' x

prop_LikeBuiltinRepeat x = take 10 (repeat' x) == take 10 (repeat x)

and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = x && and' xs

prop_LikeBuiltinAnd x = and' x == and x

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs) = if f x
    then x : takeWhile' f xs
    else []

prop_LikeBuiltinTakeWhile x = takeWhile' smallerTen x == takeWhile smallerTen x

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (x:xs) = if f x
    then dropWhile' f xs
    else x:xs

prop_LikeBuiltinDropWhile x = dropWhile' smallerTen x == dropWhile smallerTen x

maximum' :: Ord a => [a] -> a
maximum' [x] = x
maximum' (x:y:xs) = if x > y
    then maximum' (x:xs)
    else maximum' (y:xs)

prop_LikeBuiltinMaximum x = maximum' (x++[0]) == maximum (x++[0])

return []
runTests :: IO Bool
runTests = $quickCheckAll
