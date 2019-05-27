{-# LANGUAGE OverloadedStrings #-}

module Main where

import Graphics.Svg
import qualified Sheet3.Ex3 as Ex3

main :: IO ()
main = renderToFile "output.svg" (Ex3.toSvgWithHeader 500 500 $ Ex3.scalePic 100 Ex3.houseWithRoofWindow)
