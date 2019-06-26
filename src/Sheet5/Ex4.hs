module Sheet5.Ex4 where

import MiniWhile
import ParserCon

smallExample = "x: = 0; y: = 5"

ifExample = "y: = if y> 10000 then 10000 else y fi"

exampleProgram = unlines
    ["x: = 0; y: = 5;"
    ,"while x <= 10 do"
    ,"y: = (y * 5); x: = (x + 1)"
    ,"done;"
    ,"y: = if y> 10000 then 10000 else y fi"]

-- The rest is in the MiniWhile.hs
