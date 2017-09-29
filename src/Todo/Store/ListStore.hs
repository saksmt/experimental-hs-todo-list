{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Todo.Store.ListStore where

import Prelude hiding (take, drop)
import qualified Prelude as P

import Todo.Store.Class
import Todo.Type

take = flip P.take
drop = flip P.drop

_modifyAt :: [a] -> Int -> (a -> a) -> [a]
_modifyAt todoList humanIndex f = todoList `take` index ++ [newItem] ++ todoList `drop` humanIndex
    where
        index = humanIndex - 1
        newItem = f $ todoList !! index

instance TodoStore TodoList where
    store `atIndex` idx = store !! idx
    listAll store = store
    listIncomplete = filter (not . done)
    modifyAt = _modifyAt
    store `add` value = store ++ [value]
    hasIndex store idx = length store >= idx
