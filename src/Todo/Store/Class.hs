{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE KindSignatures #-}

module Todo.Store.Class where

import Todo.Type
import Control.Monad.Except
import Control.Monad(liftM3)

type TodoStoreIdentifier = String

class (Monad m) => TodoStore (m :: * -> *) where
    atIndex :: Int -> m TodoItem
    listAll :: m [TodoItem]
    listAllIndexed :: m [(Int, TodoItem)]
    listAllIndexed = zip [1..] <$> listAll
    listIncomplete :: m [TodoItem]
    listIncomplete = filter (not . done) <$> listAll
    listIncompleteIndexed :: m [(Int, TodoItem)]
    listIncompleteIndexed = filter (not . done . snd) <$> listAllIndexed
    hasIndex :: Int -> m Bool
    hasIndex idx = elem idx . map fst <$> listAllIndexed
    add :: TodoItem -> m ()
    total :: m Int
    total = length <$> listAll

    updateAtIndex :: Int -> TodoItem -> m ()
    updateAtIndex idx newTodo = modifyAt idx $ const newTodo

    modifyAt :: Int -> (TodoItem -> TodoItem) -> m ()
    modifyAt idx upd = upd <$> atIndex idx >>= updateAtIndex idx

    {-# MINIMAL atIndex, listAll, add, updateAtIndex
              | atIndex, listAll, add, modifyAt #-}

class (Monad s, TodoStore s, Monad m) => TodoStoreAccessor (s :: * -> *) (m :: * -> *) where
    load :: TodoStoreIdentifier -> m (Except String (s ()))
    save :: TodoStoreIdentifier -> s () -> m ()

