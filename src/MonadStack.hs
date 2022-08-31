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
import Control.Applicative


type Pipe a = PipeT Identity a
type PipeT m a = FailableT (WithConfig (Logger m)) a

logMsg :: Monad m => String -> PipeT m ()
logError :: Monad m => Exception -> PipeT m ()
getConfig :: Monad m => PipeT m Config
catch :: Monad m => PipeT m a -> (Exception -> PipeT m a) -> PipeT m a
throw :: Monad m => Exception -> PipeT m a

runPipe :: Config -> Pipe a -> (Failable a, Log)
runPipe conf = runFailableT >>> flip runReaderT conf >>> runWriterT >>> runIdentity

------------------ Failable ------------------

data Exception = forall a . Show a => Exception a

data Failable a = Success a 
    | Error Exception

instance Show a => Show (Failable a) where
    show (Success a) = show a
    show (Error (Exception a)) = show a

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
            Success x -> return x 
            Error e -> handler e

instance MonadTrans FailableT where
    lift xm = FailableT $ fmap Success xm

instance Monad m => Alternative (FailableT m) where
    empty = FailableT . pure . Error $ Exception "empty alternative"
    (FailableT xm) <|> yfm = FailableT $
        xm >>= (\xf -> case xf of Success x -> return (Success x); Error e -> runFailableT yfm) 

throw = throwError
catch = catchError

------------------ Logger ------------------

data Log = Log String deriving (Eq, Show)

instance Semigroup Log where
    (Log x) <> (Log y) = Log (x ++ y)

instance Monoid Log where
    mempty = Log ""

type Logger = WriterT Log

logMsg msg = lift $ tell (Log (msg ++ "\n"))
logError (Exception e) = logMsg (show e)

------------------ Config ------------------

data Config = Config deriving (Eq, Show)

type WithConfig = ReaderT Config

getConfig = lift $ ask

