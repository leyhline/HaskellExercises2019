module Sheet2.Ex5 where

data Field = X | O | E deriving (Show, Eq)
data Grid = Grid Field Field Field Field Field Field Field Field Field deriving Show
data Row = Row Field Field Field deriving Show
type Col = Row
type Diag = Row

getRows :: Grid -> (Row, Row, Row)
getRows (Grid x1 x2 x3 y1 y2 y3 z1 z2 z3) = (Row x1 x2 x3, Row y1 y2 y3, Row z1 z2 z3)

getCols :: Grid -> (Col, Col, Col)
getCols (Grid x1 x2 x3 y1 y2 y3 z1 z2 z3) = (Row x1 y1 z1, Row x2 y2 z2, Row x3 y3 z3)

getDiags :: Grid -> (Diag, Diag)
getDiags (Grid x1 x2 x3 y1 y2 y3 z1 z2 z3) = (Row x1 y2 z3, Row x3 y2 z1)

emptyGrid = Grid E E E E E E E E E

gridToList :: Grid -> [Field]
gridToList (Grid x1 x2 x3 y1 y2 y3 z1 z2 z3) = [x1, x2, x3, y1, y2, y3, z1, z2, z3]

countFields :: Field -> Grid -> Int
countFields field grid = length $ filter (field ==) $ gridToList grid

inProgress :: Grid -> Bool
inProgress grid = (not $ isInvalid grid) && (not $ isWon grid) 

isInvalid :: Grid -> Bool
isInvalid grid = fieldsDifferTooMuch || tooManyOs || tooManyXs || noEmptyFields
    where
        fieldsDifferTooMuch = abs ((countFields X grid) - (countFields O grid)) > 1
        tooManyXs = (countFields X grid) > 5
        tooManyOs = (countFields O grid) > 5
        noEmptyFields = (countFields E grid) == 0


whoWon :: Grid -> Field
whoWon grid
    | elem X rowStatus = X
    | elem O rowStatus = O
    | otherwise = E
    where
        (r1, r2, r3) = getRows grid
        (c1, c2, c3) = getCols grid
        (d1, d2) = getDiags grid
        rowStatus = map checkRow [r1, r2, r3, c1, c2, c3, d1, d2]

isWon :: Grid -> Bool
isWon grid = whoWon grid /= E

checkRow :: Row -> Field
checkRow (Row x y z) = if x == y && x == z
    then x
    else E
