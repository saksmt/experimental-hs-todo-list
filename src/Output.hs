{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Output (OutT, Out, writeError, writeMessage, messageStreamT, messageStream, errorStreamT, errorStream, streams, streamsT, IOStreamed(..)) where

import Control.Monad.Trans
import Control.Monad.State
import Control.Applicative
import Data.Functor.Identity(Identity)
import System.IO

data OutputS a = OutputS { stdErr :: a, stdOut :: a }
type OutState u m a = StateT (OutputS a) m u
newtype OutT a m u = OutT { execOut :: OutState u m a }

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

class Monad m => MonadOutput a (m :: * -> *) where
    writeError :: a -> m ()
    writeMessage :: a -> m ()

type Out a = OutT a Identity

instance (Monad m, Monoid a) => MonadOutput a (OutT a m) where
    writeError message = OutT $ do
        (OutputS o e) <- get
        put (OutputS o (e `mappend` message))
        return ()

    writeMessage message = OutT $ do
        (OutputS o e) <- get
        put (OutputS (o  `mappend` message) e)
        return ()

instance (MonadOutput a m) => MonadOutput a (StateT b m) where
    writeError message = lift $ writeError message
    writeMessage message = lift $ writeMessage message

instance (MonadState s m, Monoid a) => MonadState s (OutT a m) where
    get = lift get
    put k = lift $ put k

infixl 5 >-
out >- v = v (execOut out) (OutputS mempty mempty)

errorStreamT :: (Monoid a, Monad m) => OutT a m v -> m a
errorStreamT out = stdErr <$> out >- execStateT
errorStream :: Monoid a => Out a v -> a
errorStream out = stdErr $ out >- execState

messageStreamT :: (Monoid a, Monad m) => OutT a m v -> m a
messageStreamT out = stdOut <$> out >- execStateT
messageStream :: Monoid a => Out a v -> a
messageStream out = stdOut $ out >- execState

streams out = out >- execState
streamsT out = out >- execStateT
runOutput out = runState (execOut out) mempty
runOutputT out = runStateT (execOut out) mempty
value out = out >- evalState

class (Show v, Monoid v) => IOStreamed v where
    toIO :: Out v a -> IO a

instance IOStreamed String where
    toIO out = do
        let (OutputS o e) = streams out
        when (mempty /= o) $ putStrLn o
        when (mempty /= e) $ hPutStrLn stderr e
        return $ value out

instance {-# OVERLAPPABLE #-} (Show v, Monoid v, Eq v) => IOStreamed v where
    toIO out = do
        let (OutputS o e) = streams out
        when (mempty /= o) $ print o
        when (mempty /= e) $ hPrint stderr e
        return $ value out
