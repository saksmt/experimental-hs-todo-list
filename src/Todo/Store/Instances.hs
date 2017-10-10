module Todo.Store.Instances where

import Control.Monad.Except
import Control.Monad.State
import Control.Monad.List
import Control.Monad.Reader
import Control.Monad.Writer

import Output.Out

import Todo.Store.Class

instance (TodoStore m) => TodoStore (ExceptT e m) where
    atIndex = lift . atIndex
    listAll = lift listAll
    listAllIndexed = lift listAllIndexed
    listIncomplete = lift listIncomplete
    listIncompleteIndexed = lift listIncompleteIndexed
    hasIndex = lift . hasIndex
    add = lift . add
    updateAtIndex idx new = lift $ updateAtIndex idx new
    modifyAt idx f = lift $ modifyAt idx f
    total = lift total

instance (TodoStore m) => TodoStore (StateT s m) where
    atIndex = lift . atIndex
    listAll = lift listAll
    listAllIndexed = lift listAllIndexed
    listIncomplete = lift listIncomplete
    listIncompleteIndexed = lift listIncompleteIndexed
    hasIndex = lift . hasIndex
    add = lift . add
    updateAtIndex idx new = lift $ updateAtIndex idx new
    modifyAt idx f = lift $ modifyAt idx f
    total = lift total

instance (TodoStore m) => TodoStore (ListT m) where
    atIndex = lift . atIndex
    listAll = lift listAll
    listAllIndexed = lift listAllIndexed
    listIncomplete = lift listIncomplete
    listIncompleteIndexed = lift listIncompleteIndexed
    hasIndex = lift . hasIndex
    add = lift . add
    updateAtIndex idx new = lift $ updateAtIndex idx new
    modifyAt idx f = lift $ modifyAt idx f
    total = lift total

instance (TodoStore m) => TodoStore (ReaderT s m) where
    atIndex = lift . atIndex
    listAll = lift listAll
    listAllIndexed = lift listAllIndexed
    listIncomplete = lift listIncomplete
    listIncompleteIndexed = lift listIncompleteIndexed
    hasIndex = lift . hasIndex
    add = lift . add
    updateAtIndex idx new = lift $ updateAtIndex idx new
    modifyAt idx f = lift $ modifyAt idx f
    total = lift total

instance (TodoStore m, Monoid s) => TodoStore (WriterT s m) where
    atIndex = lift . atIndex
    listAll = lift listAll
    listAllIndexed = lift listAllIndexed
    listIncomplete = lift listIncomplete
    listIncompleteIndexed = lift listIncompleteIndexed
    hasIndex = lift . hasIndex
    add = lift . add
    updateAtIndex idx new = lift $ updateAtIndex idx new
    modifyAt idx f = lift $ modifyAt idx f
    total = lift total

instance (TodoStore m, Monoid s) => TodoStore (OutT s m) where
    atIndex = lift . atIndex
    listAll = lift listAll
    listAllIndexed = lift listAllIndexed
    listIncomplete = lift listIncomplete
    listIncompleteIndexed = lift listIncompleteIndexed
    hasIndex = lift . hasIndex
    add = lift . add
    updateAtIndex idx new = lift $ updateAtIndex idx new
    modifyAt idx f = lift $ modifyAt idx f
    total = lift total
