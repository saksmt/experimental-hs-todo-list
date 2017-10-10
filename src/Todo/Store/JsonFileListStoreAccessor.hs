{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Todo.Store.JsonFileListStoreAccessor(TodoStoreAccessor(..)) where

import System.Directory(doesFileExist)
import Data.Aeson(encode, eitherDecode, FromJSON)
import Data.ByteString.Lazy(readFile, writeFile, ByteString)
import Control.Monad.Except
import Control.Applicative((<$>))
import Control.Monad.State
import Control.Monad.IO.Class

import Prelude hiding (readFile, writeFile)

import Todo.Type
import Todo.Store.ListStore
import Todo.Store.Class

readJsonText :: String -> IO ByteString
readJsonText file = do
    exists <- doesFileExist file
    if exists then
        readFile file
    else
        return "[]"

writeJson :: String -> TodoList -> IO ()
writeJson file value = writeFile file $ encode value

parseJson :: (MonadState TodoList m) => ByteString -> Except String (m ())
parseJson value = either throwError (return . put) decoded
    where decoded :: Either String TodoList
          decoded = eitherDecode value

instance TodoStoreAccessor (State TodoList) IO where
    load :: TodoStoreIdentifier -> IO (Except String (State TodoList ()))
    load path = parseJson <$> readJsonText path
    save :: TodoStoreIdentifier -> State TodoList () -> IO ()
    save file value = writeJson file $ execState value []

instance (Monad m, MonadIO m) => TodoStoreAccessor (StateT TodoList m) m where
    load :: TodoStoreIdentifier -> m (Except String (StateT TodoList m ()))
    load path = liftIO $ parseJson <$> readJsonText path
    save :: TodoStoreIdentifier -> StateT TodoList m () -> m ()
    save file value = do
        v <- execStateT value []
        liftIO $ writeJson file v
