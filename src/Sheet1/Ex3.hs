{-# LANGUAGE TemplateHaskell #-}
module Sheet1.Ex3 where

import Data.List
import Test.QuickCheck.All

addOne :: Int -> Int
addOne x = x + 1

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

-- concat' :: Foldable t => t [a] -> [a]

-- zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]

-- repeat' :: a -> [a]

-- and' :: Foldable t => t Bool -> Bool

-- takeWhile' :: (a -> Bool) -> [a] -> [a]

-- dropWhile' :: (a -> Bool) -> [a] -> [a]

--maximum' :: forall a. (Foldable t, Ord a) => t a -> a

return []
runTests :: IO Bool
runTests = $quickCheckAll
