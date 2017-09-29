{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}

module Todo.Store.JsonFileListStoreAccessor(TodoStoreAccessor(..)) where

import System.Directory(doesFileExist)
import Data.Aeson(encode, eitherDecode, FromJSON)
import Data.ByteString.Lazy(readFile, writeFile, ByteString)
import Control.Monad.Except
import Control.Applicative((<$>))

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

writeJson file value = writeFile file $ encode value

parseJson :: ByteString -> Except String TodoList
parseJson value = either throwError return $ eitherDecode value

instance TodoStoreAccessor TodoList IO where
    load :: TodoStoreIdentifier -> IO (Except String TodoList)
    load path = parseJson <$> readJsonText path
    save = writeJson