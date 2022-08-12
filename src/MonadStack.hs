{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}
module MonadStack where

import Control.Monad
import Control.Monad.Writer.Lazy
import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.Except
import Control.Arrow


type Pipe a = FailableT (WithConfig (Logger Identity)) a

logMsg :: String -> Pipe ()
logError :: Exception -> Pipe ()
getConfig :: Pipe Config
catch :: Pipe a -> (Exception -> Pipe a) -> Pipe a
throw :: Exception -> Pipe a

runPipe :: Config -> Pipe a -> (Failable a, Log)
runPipe conf = runFailableT >>> flip runReaderT conf >>> runWriterT >>> runIdentity

------------------ Failable ------------------

data Exception = forall a . Show a => Exception a

data Failable a = Success a 
    | Error Exception

newtype FailableT m a =
    FailableT {runFailableT :: m (Failable a)}

instance Monad m => Monad (FailableT m) where
    return = FailableT . return . Success
    xm >>= f = FailableT $ do
                xf <- runFailableT xm
                case xf of
                    Error e -> return (Error e)
                    Success x -> runFailableT $ f x

instance Monad m => Applicative (FailableT m) where
    pure = return
    (<*>) = ap

instance Monad m => Functor (FailableT m) where
    fmap = liftM

instance Monad m => MonadError Exception (FailableT m) where
    throwError e = FailableT . return $ Error e
    catchError xm handler = FailableT $ do
        xf <- runFailableT xm
        runFailableT $ case xf of
            Error e -> handler e
            _ -> xm

instance MonadTrans FailableT where
    lift xm = FailableT $ fmap Success xm

throw = throwError
catch = catchError

------------------ Logger ------------------

data Log = Log String deriving (Eq, Show, Semigroup, Monoid)

type Logger = WriterT Log

logMsg msg = lift $ tell (Log msg)
logError (Exception e) = logMsg (show e)

------------------ Config ------------------

data Config = Config

type WithConfig = ReaderT Config

getConfig = lift $ ask

