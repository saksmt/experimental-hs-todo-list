{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Output.Stream(
    OutputStream(..)
) where

import Output.Out(Out, OutT, OutputS(..), runOutput, runOutputT)
import Control.Monad(when, join)
import Control.Monad.Trans
import System.IO(hPutStrLn, hPrint, stderr)

class (Show v, Monoid v) => OutputStream v where
    toIO :: Out v a -> IO a
    toIOT :: (Monad m) => OutT v m a -> m (IO a)

instance OutputStream String where
    toIO out = do
        let (v, OutputS o e) = runOutput out
        when (mempty /= o) $ putStrLn o
        when (mempty /= e) $ hPutStrLn stderr e
        return v

    toIOT out = do
        (v, OutputS o e) <- runOutputT out
        return $ do
            when (mempty /= o) $ putStrLn o
            when (mempty /= e) $ hPutStrLn stderr e
            return v

instance {-# OVERLAPPABLE #-} (Show v, Monoid v, Eq v) => OutputStream v where
    toIO out = do
        let (v, OutputS o e) = runOutput out
        when (mempty /= o) $ print o
        when (mempty /= e) $ hPrint stderr e
        return v

    toIOT out = do
        (v, OutputS o e) <- runOutputT out
        return $ do
            when (mempty /= o) $ print o
            when (mempty /= e) $ hPrint stderr e
            return v
