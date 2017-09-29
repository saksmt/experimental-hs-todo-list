module Todo.Actions(
    TodoAction,
    prettyPrintA,
    markDoneA,
    markUnDoneA,
    toggleDoneA,
    addTodoA,
    editTodoA
) where

import Control.Monad.State
import Control.Monad
import Control.Monad.Except

import Todo.Pure
import Output(OutT, (<<), out)
import Todo.Type
import Todo.Store.Class

type PersistedTodoT a m s = StateT s m a
type TodoAction a s = PersistedTodoT a (OutT String (Except String)) s


modifyTodos :: TodoStore s => (s -> s) -> TodoAction s s
modifyTodos f = do
    modify f
    get

withTodos :: TodoStore s => (s -> TodoAction a s) -> TodoAction s s
withTodos f = do
    todos <- get
    f todos
    return todos

prettyPrintA :: TodoStore s => Bool -> TodoAction s s
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
