{-# LANGUAGE OverloadedStrings #-}

module Main where

import Sheet3.Ex3(createSvg)

main :: IO ()
main = createSvg "output.svg" 15
