module Sheet7.Ex1 where

import Control.Monad.Trans
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer.Strict

data ProtectedData a = ProtectedData String a

accessData :: String -> ProtectedData a -> Maybe a
accessData s (ProtectedData pass v) = if s == pass then Just v else Nothing

type Protected s a = MaybeT (Reader (ProtectedData s)) a

run :: ProtectedData s -> Protected s a -> Maybe a
run pData protected = runReader (runMaybeT protected) pData

access :: String -> Protected a a
access pass = MaybeT $ do
    pData <- ask
    let decrypted = accessData pass pData
    return decrypted
