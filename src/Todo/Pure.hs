module Todo.Pure(
    TodoItem(..),
    TodoList,
    prettyPrint,
    markDone,
    markUnDone,
    toggleDone,
    addTodo,
    editTodo
) where

import Control.Monad.State(StateT)
import Output(Out)
import qualified Prelude as P
import Prelude hiding (take, drop)

data TodoItem = TodoItem {
    text :: String,
    done :: Bool
} deriving (Eq, Show)

type TodoList = [TodoItem]

prettyPrint :: Bool -> TodoList -> String
prettyPrint showCompleted list = let
    filterCompleted = if showCompleted then const True else not . done
    separator = "\n"
    doneFlag complete = if complete then "x" else " "
    line n (TodoItem text done) = prefix ++ content ++ separator
        where prefix = show n ++ ". [" ++ doneFlag done ++ "] "
              content = text
        in concatMap (uncurry line) $ filter (filterCompleted . snd) $ zip [1..] list

markDone index list = list `modifyAt` index $ \x -> x { done = True }
markUnDone index list = list `modifyAt` index $ \x -> x { done = False }
toggleDone index list = list `modifyAt` index $ \x -> x { done = not $ done x }

addTodo newItem todos = todos ++ [TodoItem { text = newItem, done = False }]

editTodo index newText todos = todos `modifyAt` index $ \x -> x { text = newText }

take = flip P.take
drop = flip P.drop

modifyAt :: [a] -> Int -> (a -> a) -> [a]
modifyAt todoList humanIndex f = todoList `take` index ++ [newItem] ++ todoList `drop` humanIndex
    where
        index = humanIndex - 1
        newItem = f $ todoList !! index
