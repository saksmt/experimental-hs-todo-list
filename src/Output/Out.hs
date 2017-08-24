{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Output.Out(
    OutT,
    Out,
    OutputS(..),

    errorStream,
    errorStreamT,

    messageStream,
    messageStreamT,

    value,
    valueT,

    runOutput,
    runOutputT,

    streams,
    streamsT
) where

import Control.Monad.Trans
import Control.Monad.State
import Control.Applicative
import Control.Monad.Except
import Data.Functor.Identity(Identity)
import Output.MonadOutput

data OutputS a = OutputS { stdErr :: a, stdOut :: a }
type OutState u m a = StateT (OutputS a) m u

newtype OutT a m u = OutT { execOut :: OutState u m a }
type Out a = OutT a Identity

instance (Monad m, Monoid a) => Monad (OutT a m) where
    return x = OutT $ return x
    out >>= f = OutT $ do
        x <- execOut out
        execOut $ f x
instance MonadTrans (OutT a) where
    lift out = OutT $ lift out

instance Monad m => Functor (OutT a m) where
    fmap f m = OutT $ f <$> execOut m

instance Monad m => Applicative (OutT a m) where
    pure x = OutT $ pure x
    f <*> m = OutT $ execOut f <*> execOut m


instance (Monad m, Monoid a) => MonadOutput a (OutT a m) where
    writeError message = OutT $ do
        (OutputS o e) <- get
        put (OutputS o (e `mappend` message))
        return ()

    writeMessage message = OutT $ do
        (OutputS o e) <- get
        put (OutputS (o  `mappend` message) e)
        return ()

instance (MonadState s m, Monoid a) => MonadState s (OutT a m) where
    get = lift get
    put = lift . put

-- Unpacking layers till content of State, then using `catchE` and packing all back
liftCatch catchE m h = OutT $ StateT $ \s -> runStateT (execOut m) s `catchE` \e -> runStateT (execOut $ h e) s

instance (MonadError e m, Monoid a) => MonadError e (OutT a m) where
    throwError = lift . throwError
    catchError = liftCatch catchError

infixl 5 >-
out >- v = v (execOut out) (OutputS mempty mempty)

errorStreamT out = stdErr <$> out >- execStateT
errorStream out = stdErr $ out >- execState

messageStreamT out = stdOut <$> out >- execStateT
messageStream out = stdOut $ out >- execState

streams out = out >- execState
streamsT out = out >- execStateT

runOutput out = out >- runState
runOutputT out = out >- runStateT

value out = out >- evalState
valueT out = out >- evalStateT
