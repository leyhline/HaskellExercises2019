module Sheet3.Ex2 where

import Sheet3.Ex1

instance Num a => Monoid (Vector a) where
    mempty = Vector 0 0
instance Num a => Semigroup (Vector a) where
    v <> w = v + w
