module Sheet4.Ex4 where

import qualified Data.Map as Map

data Trie = Trie Bool (Map.Map Char Trie)
    deriving Show

empty :: Trie
empty = Trie False Map.empty

insert :: [Char] -> Trie -> Trie
insert = undefined

member :: [Char] -> Trie -> Bool
member = undefined

prefix :: [Char] -> Trie -> Trie
prefix = undefined

union :: Trie -> Trie -> Trie
union = undefined

ofList :: [[Char]] -> Trie
ofList = undefined
