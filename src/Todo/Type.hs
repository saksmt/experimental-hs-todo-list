{-# LANGUAGE DeriveGeneric #-}

module Todo.Type(TodoItem(..), TodoList(..)) where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics(Generic)

data TodoItem = TodoItem {
    text :: String,
    done :: Bool
} deriving (Eq, Show, Generic)

instance ToJSON TodoItem
instance FromJSON TodoItem

type TodoList = [TodoItem]
