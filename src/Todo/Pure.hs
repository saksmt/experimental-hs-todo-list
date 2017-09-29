module Todo.Pure(
    prettyPrint,
    markDone,
    markUnDone,
    toggleDone,
    addTodo,
    editTodo
) where

import Todo.Type
import Todo.Store.Class

prettyPrint :: (TodoStore s) => Bool -> s -> String
prettyPrint showCompleted store = let
    filterCompleted = if showCompleted then const True else not . done
    separator = "\n"
    doneFlag complete = if complete then "x" else " "
    line n (TodoItem text done) = prefix ++ content ++ separator
        where prefix = show n ++ ". [" ++ doneFlag done ++ "] "
              content = text
        in concatMap (uncurry line) $ filter (filterCompleted . snd) $ zip [1..] $ listAll store

markDone index store = store `modifyAt` index $ \x -> x { done = True }
markUnDone index store = store `modifyAt` index $ \x -> x { done = False }
toggleDone index store = store `modifyAt` index $ \x -> x { done = not $ done x }

addTodo newItem store = store `add` TodoItem { text = newItem, done = False }
editTodo index newText store = store `modifyAt` index $ \x -> x { text = newText }
