{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

module Todo.Store.ListStore where

import Prelude hiding (take, drop)
import qualified Prelude as P

import Control.Monad.State

import Todo.Store.Class
import Todo.Type

take = flip P.take
drop = flip P.drop

_modifyAt :: [a] -> Int -> (a -> a) -> [a]
_modifyAt todoList humanIndex f = todoList `take` index ++ [newItem] ++ todoList `drop` humanIndex
    where
        index = humanIndex - 1
        newItem = f $ todoList !! index

instance (Monad m, MonadState TodoList m) => TodoStore m where
    atIndex idx = (!! idx) <$> get
    listAll = get
    modifyAt idx f = modify $ \x -> _modifyAt x idx f
    add value = modify $ \x -> x ++ [value]
    hasIndex idx = (<= idx) . length <$> get
