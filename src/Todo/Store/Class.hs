{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}

module Todo.Store.Class where

import Todo.Type
import Control.Monad.Except

type TodoStoreIdentifier = String

class TodoStore a where
    atIndex :: a -> Int -> TodoItem
    listAll :: a -> [TodoItem]
    listIncomplete :: a -> [TodoItem]
    hasIndex :: a -> Int -> Bool
    add :: a -> TodoItem -> a

    updateAtIndex :: a -> Int -> TodoItem -> a
    updateAtIndex store idx newTodo = store `modifyAt` idx $ const newTodo

    modifyAt :: a -> Int -> (TodoItem -> TodoItem) -> a
    modifyAt store idx upd = store `updateAtIndex` idx $ upd $ store `atIndex` idx

    {-# MINIMAL atIndex, listAll, hasIndex, listIncomplete, add, updateAtIndex
              | atIndex, listAll, hasIndex, listIncomplete, add, modifyAt #-}

class (TodoStore a, Monad m) => TodoStoreAccessor a (m :: * -> *) where
    load :: TodoStoreIdentifier -> m (Except String a)
    save :: TodoStoreIdentifier -> a -> m ()
