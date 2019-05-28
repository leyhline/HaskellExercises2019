module Sheet3.Ex3 where

import Data.List
import System.Random
import Graphics.Svg
import qualified Data.Text as T

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
houseWithRoofWindow = Picture [Rectangle 0 0 1 1, Line 0 0 1 1, Line 0 1 1 0, Line 0 1 0.5 2, Line 0.5 2 1 1, Circle 0.5 1.30 0.25]

attr :: AttrTag -> Float -> Attribute
attr a x = a <<- T.pack (show x)

toSvg :: Picture -> Element
toSvg (Line x1 y1 x2 y2) = line_ [attr X1_ x1, attr Y1_ y1, attr X2_ x2, attr Y2_ y2]
toSvg (Rectangle x y width height) = rect_ [attr X_ x, attr Y_ y, attr Width_ width, attr Height_ height]
toSvg (Circle cx cy r) = circle_ [attr Cx_ cx, attr Cy_ cy, attr R_ r]
toSvg (Picture []) = mempty
toSvg (Picture (p:ps)) = toSvg p <> toSvg (Picture ps)

normalize :: Float -> Float -> Element -> Element
normalize width height = g_ [Stroke_ <<- T.pack "black", Fill_ <<- T.pack "none", Transform_ <<- rotate 180 <> translate (negate width) (negate height)]

toSvgWithHeader :: Float -> Float -> Picture -> Element
toSvgWithHeader canvasWidth canvasHeight pic = doctype <> with (svg11_ (normalize canvasWidth canvasHeight $ toSvg pic)) [attr Width_ canvasWidth, attr Height_ canvasHeight]

movePic :: (Float, Float) -> Picture -> Picture
movePic (xt, yt) (Line x1 y1 x2 y2) = Line (xt+x1) (yt+y1) (xt+x2) (yt+y2)
movePic (xt, yt) (Rectangle x y width height) = Rectangle (xt+x) (yt+y) width height
movePic (xt, yt) (Circle cx cy r) = Circle (xt+cx) (yt+cy) r
movePic movement (Picture p) = Picture (map (movePic movement) p)

scalePic :: Float -> Picture -> Picture
scalePic s (Line x1 y1 x2 y2) = Line (s*x1) (s*y1) (s*x2) (s*y2)
scalePic s (Rectangle x y width height) = Rectangle (s*x) (s*y) (s*width) (s*height)
scalePic s (Circle cx cy r) = Circle (s*cx) (s*cy) (s*r)
scalePic s (Picture p) = Picture (map (scalePic s) p)

scaleAndMoveHorizontal :: Float -> Float -> Picture -> Picture
scaleAndMoveHorizontal scaleFactor movement = movePic (movement, 0) . scalePic scaleFactor

randomNumbers :: (Float, Float) -> Int -> IO [Float]
randomNumbers range nr = take nr . randomRs range <$> getStdGen

randomHouses :: Int -> IO Picture
randomHouses nr = do
    someNumbers <- randomNumbers (0.25, 4.0) nr
    return $ Picture $ snd $ mapAccumR (\mov scal -> (mov+scal, scaleAndMoveHorizontal scal mov houseWithRoofWindow)) 0.0 someNumbers

createSvgHouses :: String -> Int -> IO ()
createSvgHouses  filename nrOfHouses = do
    houses <- randomHouses nrOfHouses
    renderToFile filename (toSvgWithHeader (120 * fromIntegral nrOfHouses) 500 $ scalePic 50 houses)

rotateAroundCenter :: Float -> Picture -> Picture
rotateAroundCenter degree (Line x1 y1 x2 y2) = uncurry (uncurry Line (rot (x1, y1) degree)) (rot (x2, y2) degree)
rotateAroundCenter degree (Rectangle x y width height) = uncurry Rectangle (rot (x, y) degree) width height
rotateAroundCenter degree (Circle cx cy r) = uncurry Circle (rot (cx, cy) degree) r
rotateAroundCenter degree (Picture pics) = Picture (map (rotateAroundCenter degree) pics)

rotatePic :: (Float, Float) -> Float -> Picture -> Picture
rotatePic (c1, c2) degree = movePic (c1, c2) . rotateAroundCenter degree . movePic (negate c1, negate c2)

rot :: (Float, Float) -> Float -> (Float, Float)
rot (x, y) degree = (r2*x - r1*y, r1*x + r2*y)
    where
        r1 = sin (degree * pi / 180)
        r2 = cos (degree * pi / 180)

dragon :: Integer -> Picture
dragon i
    | i < 0 = error "This won't take negative numbers."
    | i == 0 = Picture [Line 0 0 0 50]
    | otherwise = newDragon <> formerDragon
    where
        formerDragon = dragon (i-1)
        c1 = (if even i then x2 else x1) $ head $ getPictureList formerDragon
        c2 = (if even i then y2 else y1) $ head $ getPictureList formerDragon
        newDragon = rotatePic (c1, c2) 90 formerDragon
        

getPictureList :: Picture -> [Picture]
getPictureList (Picture p) = p

createSvgDragon :: String -> Integer -> IO ()
createSvgDragon filename i = renderToFile filename (toSvgWithHeader 500 500 $ movePic (100, 100) $ dragon i)
