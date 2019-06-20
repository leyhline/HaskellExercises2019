module Sheet5.Ex3 where

import Data.Char
import ParserCon

pmany :: Parser t r -> Parser t [r]
pmany p = ((:) <$> p <*> pmany p) <|> pure []

pmany1 :: Parser t r -> Parser t [r]
pmany1 p = (:) <$> p <*> pmany p

pIntersperse :: Parser t r -> Parser t w -> Parser t [r]
pIntersperse pThing pSep = undefined

pInt :: Parser Char Integer
pInt = read <$> pmany1 (satisfy isDigit)

pIntList :: Parser Char [Integer]
pIntList = lit '[' *> pmany p <* lit ']'
    where
        p = pInt <* lit ','

pPaliAB :: Parser Char String
pPaliAB = undefined

pPali :: (Eq r) => Parser t r -> Parser t [r]
pPali = undefined

pTwice :: (Eq t) => Parser t [t] -> Parser t [t]
pTwice = undefined
