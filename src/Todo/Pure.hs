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

prettyPrint :: (Monad m, TodoStore m) => Bool -> m String
prettyPrint showCompleted = let
    list = (if showCompleted then listAllIndexed else listIncompleteIndexed)
    separator = "\n"
    doneFlag complete = if complete then "x" else " "
    line n (TodoItem text done) = prefix ++ content ++ separator
        where prefix = show n ++ ". [" ++ doneFlag done ++ "] "
              content = text
        in concatMap (uncurry line) <$> list

markDone index = modifyAt index $ \x -> x { done = True }
markUnDone index = modifyAt index $ \x -> x { done = False }
toggleDone index = modifyAt index $ \x -> x { done = not $ done x }

addTodo newItem = add TodoItem { text = newItem, done = False }
editTodo index newText = modifyAt index $ \x -> x { text = newText }
