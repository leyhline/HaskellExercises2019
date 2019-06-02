{-# LANGUAGE TemplateHaskell #-}
module Sheet4.Ex4 where

import Test.QuickCheck.All
import qualified Data.Map as Map
import qualified Data.Set as Set

data Trie = Trie Bool (Map.Map Char Trie)

instance Show Trie where
    show (Trie isWord children) = prefix ++ Map.foldrWithKey (\k v former -> k : '→' : '[' : show v ++ "]" ++ former) "" children
        where prefix = if isWord then "↲" else ""

empty :: Trie
empty = Trie False Map.empty

insert :: [Char] -> Trie -> Trie
insert [] (Trie _ subnodes) = Trie True subnodes
insert (x:xs) (Trie isWord children) = Trie isWord (Map.insertWith insertExisting x newTrie children)
    where
        newTrie = insert xs empty
        insertExisting _ = insert xs

member :: [Char] -> Trie -> Bool
member [] (Trie isWord _) = isWord
member (x:xs) (Trie _ children) = case Map.lookup x children of
    Nothing -> False
    Just trie -> member xs trie

prefix :: [Char] -> Trie -> Trie
prefix [] trie = trie
prefix (x:xs) trie = Trie False (Map.singleton x (prefix xs trie))

union :: Trie -> Trie -> Trie
union (Trie isWord1 children1) (Trie isWord2 children2) = Trie (isWord1 || isWord2) newMap
    where
        allKeys = Map.keysSet children1 `Set.union` Map.keysSet children2
        newMap = Map.fromList $ Set.foldr (\k former -> (k, getTrie k) : former) [] allKeys
        getTrie k = case (Map.lookup k children1, Map.lookup k children2) of
            (Nothing, Just y) -> y
            (Just x, Nothing) -> x
            (Just x, Just y) -> x `union` y

ofList :: [[Char]] -> Trie
ofList = foldr insert empty

prop_CheckIfInsertedWordsAreMembers :: [[Char]] -> Bool
prop_CheckIfInsertedWordsAreMembers words = foldr (\word isMember -> isMember && member word trie) True words
    where trie = ofList words

prop_CheckIfInsertedWordsWithPrefixAreMembers :: [Char] -> [[Char]] -> Bool
prop_CheckIfInsertedWordsWithPrefixAreMembers pre words = foldr (\word isMember -> isMember && member (pre++word) trie) True words
    where trie = prefix pre (ofList words)

prop_CheckMembersForMergedTrie :: [[Char]] -> [[Char]] -> Bool
prop_CheckMembersForMergedTrie words1 words2 = foldr (\word isMember -> isMember && member word trie) True (words1 ++ words2)
    where trie = ofList words1 `union` ofList words2

return []
runTests :: IO Bool
runTests = $quickCheckAll
