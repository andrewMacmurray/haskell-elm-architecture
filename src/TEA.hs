{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.Trans       (liftIO)
import           Control.Monad.Trans.State (StateT, evalStateT, get, put)
import           Data.Text                 (Text, toUpper)
import qualified Data.Text.IO              as T
import           System.Directory          (getCurrentDirectory)

data Msg =
    AddOne
  | AddTwo
  | MinusOne
  | MinusTwo
  | SayHello
  | CurrentDir
  | NoOp
  deriving (Eq, Show)

type Model = Integer

data Cmd =
    None
  | One (IO ())


main :: IO ()
main = do
  putStrLn "Enter a Command"
  evalStateT appState 0


parseMsg :: Text -> Msg
parseMsg msg =
  case toUpper msg of
    "ADDONE"     -> AddOne
    "ADDTWO"     -> AddTwo
    "MINUSONE"   -> MinusOne
    "MINUSTWO"   -> MinusTwo
    "SAYHELLO"   -> SayHello
    "CURRENTDIR" -> CurrentDir
    _            -> NoOp


appState :: StateT Model IO ()
appState = do
  msg   <- liftIO $ parseMsg <$> T.getLine
  model <- get
  let (newM, cmd) = update msg model
  liftIO $ do
    putStrLn $ "msg was: "   ++ show msg
    putStrLn $ "new model: " ++ show newM
    processCmd cmd
  put newM
  appState


processCmd :: Cmd -> IO ()
processCmd None    = return ()
processCmd (One x) = x


update :: Msg -> Model -> (Model, Cmd)
update msg model =
  case msg of
    AddOne     -> (model + 1, None)
    AddTwo     -> (model + 2, None)
    MinusOne   -> (model - 1, None)
    MinusTwo   -> (model - 2, None)
    SayHello   -> (model, One sayHello)
    CurrentDir -> (model, One printCurrentDir)
    NoOp       -> (model, None)


sayHello :: IO ()
sayHello = putStrLn "Hello I'm a Cmd!"


printCurrentDir :: IO ()
printCurrentDir =
  ("Current Dir is: " ++) <$> getCurrentDirectory >>= putStrLn
