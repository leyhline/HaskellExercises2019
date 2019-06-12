module Sheet5.Ex3 where

import Control.Applicative
import Control.Monad

-- Definitions from lecture:

newtype Parser token result = Parser {runParser :: [token] -> [(result, [token])]}
    
succeed :: r -> Parser t r
succeed r = Parser $ \ ts -> [(r, ts)]

pseq :: Parser t (s -> r) -> Parser t s -> Parser t r
pseq p1 p2 = Parser $ \ts ->
    let srs = runParser p1 ts -- :: [(s -> r, [t])]
        g (srf, ts1) = map (h srf) $ runParser p2 ts1 -- [(s, [t])]
        h srf (s, ts2) = (srf s, ts2)
        rs  = map g srs -- [[(r,[t])]]
    in concat rs

pempty :: Parser t r
pempty = Parser $ const []

palt :: Parser t r -> Parser t r -> Parser t r
palt p1 p2 =
    Parser $ \ts -> runParser p1 ts ++ runParser p2 ts

pmap :: (s -> r) -> Parser t s -> Parser t r
pmap f p = Parser (map g . runParser p)
    where g (s, ts) = (f s, ts)

instance Functor (Parser token) where
    fmap = pmap

instance Applicative (Parser token) where
    pure = succeed
    (<*>) = pseq

instance Alternative (Parser token) where
    empty = pempty
    (<|>) = palt

instance Monad (Parser token) where
    return = succeed
    (Parser p) >>= f =
        Parser (concatMap g . p)
        where g (a, ts) = runParser (f a) ts

instance MonadPlus (Parser t) where
    mzero = pempty
    mplus = palt

-- Actual exercise:

pmany :: Parser t r -> Parser t [r]
pmany = undefined

pmany1 :: Parser t r -> Parser t [r]
pmany1 = undefined

pIntList :: Parser Char [Integer]
pIntList = undefined

pPaliAB :: Parser Char String
pPaliAB = undefined

pPali :: (Eq r) => Parser t r -> Parser t [r]
pPali = undefined

pTwice :: (Eq t) => Parser t [t] -> Parser t [t]
pTwice = undefined
