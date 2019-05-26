module Sheet3.Ex3 where

import Graphics.Svg

data Picture = Line { x1 :: Float, y1 :: Float, x2 :: Float, y2 :: Float }
             | Rectangle { x :: Float, y :: Float, width :: Float, height :: Float }
             | Circle { cx :: Float, cy :: Float, r :: Float }
             | Picture [Picture]
    deriving Show

instance Monoid Picture where
    mempty = Picture []
instance Semigroup Picture where
    (Picture p1) <> (Picture p2) = Picture (p1 ++ p2)
    (Picture p1) <> p = Picture (p1 ++ [p])
    p <> (Picture p2) = Picture (p : p2)
    p1 <> p2 = Picture [p1, p2]

rectWithTriangle = Picture [Rectangle 0 0 1 1, Line 0 0 0.5 1, Line 0.5 1 1 0]
houseWithRoofWindow = Picture [Rectangle 0 0 1 1, Line 0 0 1 1, Line 0 1 1 0, Line 0 1 0.5 2, Line 0.5 2 1 1, Circle 0.5 1.25 0.25]
