{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Console (Console, IOAction, writeError, writeMessage, interpretIO) where

import System.IO
import Control.Monad
import Control.Applicative
import Control.Monad.Trans.Free

data IOAction m next = Print m next | Error m next deriving Functor

type Console a b = Free (IOAction a) b

writeError msg = liftF $ Error msg
writeMessage msg = liftF $ Print msg

interpretIO_ :: (Show a) => Free (IOAction a) b -> IO b
interpretIO_ console = case runFree console of
    Pure r -> return r
    Free (Print line next) -> do
        print line
        interpretIO_ next
    Free (Error line next) -> do
        hPrint stderr line
        interpretIO_ next

-- this shit is not working...

interpretIO :: (Show a) => Console a b -> IO b
interpretIO console = interpretIO_ console
