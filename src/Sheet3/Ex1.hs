module Sheet3.Ex1 where

data Vector a = Vector a a deriving Show
instance Num a => Num (Vector a) where
    (Vector v1 v2) + (Vector w1 w2) = Vector (v1 + w1) (v2 + w2) 
    (Vector v1 v2) - (Vector w1 w2) = Vector (v1 - w1) (v2 - w2)
    (Vector v1 v2) * (Vector w1 w2) = Vector (v1 * w1) (v2 * w2)
    abs (Vector v1 v2) = Vector (abs v1) (abs v2)
    signum (Vector v1 v2) = Vector (signum  v1) (signum v2)
    fromInteger x = Vector (fromInteger x) (fromInteger x)
