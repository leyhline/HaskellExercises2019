{-# LANGUAGE TemplateHaskell #-}
module Sheet4.Ex2 where

import Data.List(unfoldr, iterate)
import Test.QuickCheck.All

unfoldr' :: (b -> Maybe (a, b)) -> b -> [a]
unfoldr' f x = case f x of
    Nothing -> []
    Just (y, z) -> y : unfoldr' f z

countdown b = if b <= 0 then Nothing else Just (b, b-1)
prop_CompareWithBuiltinUnfoldr x = unfoldr' countdown x == unfoldr countdown x

map' :: (a -> b) -> [a] -> [b]
map' f = unfoldr' maybeMap
    where
        maybeMap [] = Nothing
        maybeMap (x:xs) = Just (f x, xs)

prop_CompareWithBuiltinMap x = map' (+ 1) x == map (+ 1) x

iterate' :: (a -> a) -> a -> [a]
iterate' f = unfoldr' (\x -> Just (x, f x))

prop_CompareWithBuiltinIterate x = take 20 (iterate' (* 2) x) == take 20 (iterate (* 2) x) 

return []
runTests :: IO Bool
runTests = $quickCheckAll
