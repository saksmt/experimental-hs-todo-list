module Todo.Actions(
    TodoAction,
    prettyPrintA,
    markDoneA,
    markUnDoneA,
    toggleDoneA,
    addTodoA,
    editTodoA
) where

import Todo.Pure
import Output(OutT, (<<), out)
import Control.Monad.State
import Control.Monad
import Control.Monad.Except

type PersistedTodoT a m = StateT TodoList m a
type TodoAction a = PersistedTodoT a (OutT String (Except String))


modifyTodos :: (TodoList -> TodoList) -> TodoAction TodoList
modifyTodos f = do
    modify f
    get

withTodos :: (TodoList -> TodoAction a) -> TodoAction TodoList
withTodos f = do
    todos <- get
    f todos
    return todos

prettyPrintA :: Bool -> TodoAction TodoList
prettyPrintA showCompleted = withTodos $ \todos ->
    out << prettyPrint showCompleted todos

checkIndex index = withTodos $ \todos ->
    when (length todos < index) $ throwError $ concat [
        "No todo at index: ", show index, "\n",
        "Last todo has index: ", show $ length todos
        ]

markDoneA index = checkIndex index >> modifyTodos (markDone index)
markUnDoneA index = checkIndex index >> modifyTodos (markUnDone index)
toggleDoneA index = checkIndex index >> modifyTodos (toggleDone index)

addTodoA text = modifyTodos $ addTodo text
editTodoA index newText = checkIndex index >> modifyTodos (editTodo index newText)
