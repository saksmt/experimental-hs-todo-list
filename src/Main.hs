{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Functor.Identity
import Data.Bifunctor
import Control.Monad.State
import System.IO(hPutStrLn, stderr)
import Data.ByteString.Lazy(readFile, writeFile, ByteString)
import Control.Monad.Except
import Output(toIOT, out, (<<), OutT)
import Todo.Pure(TodoItem(..), TodoList)
import Todo.Actions
import Options.Applicative
import Control.Monad((=<<), (>=>))
import System.Directory(doesFileExist)
import Data.Semigroup((<>))
import Data.Aeson(encode, eitherDecode)

import Prelude hiding (readFile, writeFile)

data CmdParams =
    AddTodo { text :: String } |
    EditTodo { index :: Int, text :: String } |
    Print { showCompleted :: Bool } |
    MarkDone { index :: Int } |
    MarkUnDone { index :: Int } |
    ToggleDone { index :: Int } deriving (Show)

_todoText = argument str $ metavar "TEXT" <> help "Todo text"
_todoIndex = argument auto $ metavar "INDEX" <> help "Todo index"

addTodoC = AddTodo <$> _todoText
editTodoC = EditTodo <$> _todoIndex <*> _todoText
markDoneC = MarkDone <$> _todoIndex
markUnDoneC = MarkUnDone <$> _todoIndex
toggleDoneC = ToggleDone <$> _todoIndex
printTodoC = Print <$> switch (mconcat
    [ long "show-all"
    , long "print-all"
    , long "show-completed"
    , long "print-completed"
    , short 'a'
    , help "Show all todos, even completed one's" ])

todoAppCommands = subparser $ mconcat
    [ command "create" $ info addTodoC    $ progDesc "Create new todo"
    , command "add"    $ info addTodoC    $ progDesc "Create new todo (alias for add)"
    , command "edit"   $ info editTodoC   $ progDesc "Edit todo by index"
    , command "done"   $ info markDoneC   $ progDesc "Mark todo not done"
    , command "undone" $ info markUnDoneC $ progDesc "Mark todo done"
    , command "toggle" $ info markUnDoneC $ progDesc "Toggle todo done state"
    , command "list"   $ info printTodoC  $ progDesc "Show todos"
    , command "show"   $ info printTodoC  $ progDesc "Show todos (alias for list)"
    , command "print"  $ info printTodoC  $ progDesc "Show todos (alias for list)"]

todoAppOptions = option str $
       metavar "FILE"
    <> long "todo-file"
    <> long "file"
    <> long "db"
    <> long "database"
    <> short 'd'
    <> short 'f'
    <> help "Todo database file path"
    <> showDefault
    <> value "/home/smt/.todos"

todoApp = info (((,) <$> todoAppCommands <*> todoAppOptions) <**> helper) $ fullDesc <> header "todos - simple cmd todo manager"

evalCommand (AddTodo text)        = addTodoA text
evalCommand (EditTodo index text) = editTodoA index text
evalCommand (Print all)           = prettyPrintA all
evalCommand (MarkDone index)      = markDoneA index
evalCommand (MarkUnDone index)    = markUnDoneA index
evalCommand (ToggleDone index)    = toggleDoneA index
evalCommand _                     = throwError "Unknown command: tell developer that he is the dick"

readJsonText file = do
    exists <- doesFileExist file
    if exists then
        readFile file
    else
        return "[]"

writeJson file value = writeFile file $ encode value

parseJson value = either throwError return $ eitherDecode value

runApp action file = db >>= ioAction
    where saveResults = either (hPutStrLn stderr) (writeJson file =<<)
          ioAction = saveResults . runExcept . toIOT . (parseJson >=> evalStateT action)
          db = readJsonText file

main = execParser todoApp >>= uncurry (runApp . evalCommand)