{-# LANGUAGE OverloadedStrings #-}

module Main where

import Sheet3.Ex3(createSvgHouses)

main :: IO ()
main = createSvgHouses "output.svg" 15
