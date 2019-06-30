module Sheet7.Ex2 where

import Control.Monad.Writer.Strict

data Item = Msg String
          | Section String [Item]
          deriving (Show, Eq)

type Log = [Item]

type Logging a = Writer Log a

log :: Show t => t -> Logging ()
log s = let message = Msg $ show s in
    writer ((), [message])

withSection :: String -> Logging a -> Logging a
withSection sectionName logging =
    let (a, log) = runWriter logging in
        writer (a, [Section sectionName log])

runLogging :: Logging a -> (a, Log)
runLogging = runWriter
