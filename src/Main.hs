{-# LANGUAGE FlexibleContexts #-}

module Main where

import Data.Functor.Identity
import Data.Bifunctor
import Control.Monad.State
import Output

data TodoItem = TodoItem {
    text :: String,
    done :: Bool
} deriving (Eq, Show)

type TodoList = [TodoItem]

type PersistedTodoT a m = StateT TodoList m a

type TestPersistedTodo a = PersistedTodoT a Identity

type TodoApp = PersistedTodoT TodoList (Out String)

test :: Bool -> TodoApp
test complete = do
    todos <- get
    writeMessage $ prettyPrint complete todos
    writeMessage "Hello!@"
    return todos

data Fatal = FuckUP | Unknown String

prettyPrint :: Bool -> TodoList -> String
prettyPrint showCompleted list = let
    filterCompleted = if showCompleted then const True else not . done
    separator = "\n"
    doneFlag complete = if complete then "x" else " "
    line n (TodoItem text done) = prefix ++ content ++ separator
        where prefix = show n ++ ". [" ++ doneFlag done ++ "] "
              content = text
        in concatMap (uncurry line) $ filter (filterCompleted . snd) $ zip [1..] list

prettyPrintM :: (Monad m) => Bool -> PersistedTodoT String m
prettyPrintM showCompleted = do
    todos <- get
    return $ prettyPrint showCompleted todos

markComplete index = do
    todos <- get
    return

testTodos = [
    TodoItem "Hello" False,
    TodoItem "World" True,
    TodoItem "Heh" True,
    TodoItem "Lel" False
    ]

main = do
    let result = toIO ((evalStateT $ test True) testTodos)
    t <- result
    print t
--     putStrLn $ fst $ (runState $ messageStreamT (test True)) testTodos
    return ()
--     putStrLn $ messageStream $ runStateT (test True) testTodos
--     putStrLn $ fst $ runState (prettyPrintM False)
