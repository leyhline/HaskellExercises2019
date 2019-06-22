module Sheet6.Ex1 where

import Control.Monad.Trans.State.Lazy

type Random a = State Int a

c1 = 6364136223846793005 :: Int
c2 = 1442695040888963407 :: Int

fresh :: Random Int
fresh = do
    seed <- get
    let n = c1 * seed + c2
    put n
    return n

runPRNG :: Random a -> Int -> a
runPRNG = evalState
