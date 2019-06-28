module Sheet6.Ex2 where

import Control.Monad
import Data.Map.Lazy
import Control.Monad.Trans.State.Lazy
import MiniWhile

type Name = String
type Value = Integer
type Memory = Map Name Value
type InterpM = State Memory

eval :: Program -> Memory
eval (Program stmts) = execState state empty
    where state = mapM_ evalStatement stmts

evalString :: String -> Maybe Memory
evalString s = do
    program <- parseString s
    return $ eval program

evalStatement :: Stmt -> InterpM ()
evalStatement (Asgn name exp) = do
    state <- get
    value <- evalExp exp
    put $ insert name value state
    return ()
evalStatement whileStmt@(While exp stmts) = do
    state <- get
    condition <- evalExp exp
    when (condition /= 0) (evalBody >> (evalStatement whileStmt))
    where evalBody = mapM_ evalStatement stmts

evalAExp :: AExp -> InterpM Integer
evalAExp (Num x) = return x
evalAExp (Var id) = do
    state <- get
    return $ state ! id
evalAExp (Plus x y) = evalOp (+) x y
evalAExp (Minus x y) = evalOp (-) x y
evalAExp (Mult x y) = evalOp (*) x y
evalAExp (Div x y) = evalOp div x y

evalOp:: (Integer -> Integer -> Integer) -> AExp -> AExp -> InterpM Integer
evalOp f x y = do
    xVal <- evalAExp x
    yVal <- evalAExp y
    return $ f xVal yVal

evalExp :: Exp -> InterpM Integer
evalExp (If x y z) = do
    xVal <- evalExp x
    yVal <- evalExp y
    zVal <- evalExp z
    return $ if xVal /= 0 then yVal else zVal
evalExp (LEq x y) = evalCmp (<=) x y
evalExp (GTh x y) = evalCmp (>) x y
evalExp (Equ x y) = evalCmp (==) x y
evalExp (NEq x y) = evalCmp (/=) x y
evalExp (Not x) = do
    xVal <- evalExp x
    return $ if xVal == 0 then 1 else 0
evalExp (AExp x) = evalAExp x

evalCmp :: (Integer -> Integer -> Bool) -> AExp -> AExp -> InterpM Integer
evalCmp f x y = do
    xVal <- evalAExp x
    yVal <- evalAExp y
    return $ if f xVal yVal then 1 else 0
