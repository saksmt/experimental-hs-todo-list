module Todo.Actions(
    TodoAction,
    prettyPrintA,
    markDoneA,
    markUnDoneA,
    toggleDoneA,
    addTodoA,
    editTodoA
) where

import Control.Monad
import Control.Monad.Except

import Todo.Pure
import Output(OutT, (<<), out)
import Todo.Type
import Todo.Store.Class
import Todo.Store.Instances

type TodoAction m = OutT String (ExceptT String m) ()

prettyPrintA :: (TodoStore m) => Bool -> TodoAction m
prettyPrintA showCompleted = prettyPrint showCompleted >>= (out <<)

checkIndex :: (TodoStore m) => Int -> TodoAction m
checkIndex index = whenM (hasIndex index) $ total >>= \todoTotal -> throwError $ concat [
     "No todo at index: ", show index, "\n",
     "Last todo has index: ", show todoTotal
     ]

markDoneA index = checkIndex index >> markDone index
markUnDoneA index = checkIndex index >> markUnDone index
toggleDoneA index = checkIndex index >> toggleDone index

addTodoA :: (TodoStore m) => String -> TodoAction m
addTodoA = addTodo
editTodoA index newText = checkIndex index >> editTodo index newText


whenM condition action = do
    c <- condition
    when c action