module Sheet5.Ex4 where

exampleProgram = unlines
    ["x: = 0; y: = 5;"
    ,"while x <= 10 do"
    ,"y: = (y * 5); x: = (x + 1)"
    ,"done;"
    ,"y: = if y> 10000 then 10000 else y fi"]
