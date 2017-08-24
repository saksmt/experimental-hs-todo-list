{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module Output.MonadOutput(MonadOutput(..)) where

import Control.Monad.State(StateT)
import Control.Monad.Trans(lift)
import Control.Monad.Except(ExceptT)

class Monad m => MonadOutput a (m :: * -> *) where
    writeError :: a -> m ()
    writeMessage :: a -> m ()

instance (MonadOutput a m) => MonadOutput a (StateT b m) where
    writeError message = lift $ writeError message
    writeMessage message = lift $ writeMessage message

instance (MonadOutput a m) => MonadOutput a (ExceptT b m) where
    writeError message = lift $ writeError message
    writeMessage message = lift $ writeMessage message
