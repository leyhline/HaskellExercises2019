module Sheet7.Ex2 where

import Control.Monad.Trans.Writer.Strict
import Data.Time.Clock.POSIX

data Item = Msg String
          | Section String [Item]
          deriving (Show, Eq)

type Log = [Item]

type Logging a = WriterT Log IO a

log :: Show t => t -> Logging ()
log s = WriterT $ do
    time <- getPOSIXTime
    let message = Msg $ show time ++ ": " ++ show s
    return ((), [message])

withSection :: String -> Logging a -> Logging a
withSection sectionName logging = WriterT $ do
    timeBefore <- getPOSIXTime
    (a, log) <- runLogging logging
    timeAfter <- getPOSIXTime
    return (a,
        [Msg $ show timeBefore ++ ": " ++ "SECTION START"
        ,Section sectionName log
        ,Msg $show timeAfter ++ ": " ++ "SECTION END"])

runLogging :: Logging a -> IO (a, Log)
runLogging = runWriterT
