{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Functor.Identity
import Data.Bifunctor
import Control.Monad.State
import System.IO(hPutStrLn, stderr)
import Control.Monad.Except
import Output(toIOT, out, (<<))
import Todo.Pure(TodoItem(..))
import Todo.Actions

testTodos = [
    TodoItem "1" False,
    TodoItem "2" True,
    TodoItem "3" True,
    TodoItem "4" False,
    TodoItem "5" False
    ]

testScenario = do
    prettyPrintA True
    out << "\n"
    out << "------------------------------\n"
    markDoneA 4
    toggleDoneA 2
    markUnDoneA 3
    addTodoA "6"
    editTodoA 1 "11111"
    prettyPrintA True


main = do
    let result = runExcept $ toIOT $ evalStateT testScenario testTodos
    case result of
        Left critical -> hPutStrLn stderr critical
        Right value -> do
            t <- value
            print t
