{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Applicative       ((<|>))
import           Control.Monad.Trans       (liftIO)
import           Control.Monad.Trans.State (StateT, evalStateT, get, put)
import           Data.Char                 (toLower, toUpper)
import qualified Data.Text.IO              as T
import           System.Directory          (getCurrentDirectory)
import           Text.Trifecta

data Msg =
    AddOne
  | AddTwo
  | Add Integer
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


appState :: StateT Model IO ()
appState = do
  msg   <- liftIO $ parseMsg <$> getLine
  model <- get
  let (newM, cmd) = update msg model
  liftIO $ do
    mapM_ putStrLn
      [ "msg was: "   ++ show msg
      , "new model: " ++ show newM, "" ]
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
    Add n      -> (model + n, None)
    MinusOne   -> (model - 1, None)
    MinusTwo   -> (model - 2, None)
    SayHello   -> (model, One sayHello)
    CurrentDir -> (model, One printCurrentDir)
    _          -> (model, None)


sayHello :: IO ()
sayHello = putStrLn "Hello I'm a Cmd!"


printCurrentDir :: IO ()
printCurrentDir =
  ("Current Dir is: " ++) <$> getCurrentDirectory >>= putStrLn



-- Parsers

msgParser :: Parser Msg
msgParser = try singleArgMsgs <|> try addMsg


addMsg :: Parser Msg
addMsg = Add <$> (caseInsensitive "Add" >> spaces >> integer)


singleArgMsgs :: Parser Msg
singleArgMsgs = go (map show singleMsgs)
  where go (x:xs) = stringToMsg <$> foldr ((<|>) . tci) (tci x) xs
        tci       = try . caseInsensitive


caseInsensitive :: String -> Parser String
caseInsensitive = mapM ci
  where ci x =  try (char (toLower x))
            <|> try (char (toUpper x))


singleMsgs :: [Msg]
singleMsgs =
  [ AddOne
  , AddTwo
  , MinusOne
  , MinusTwo
  , SayHello
  , CurrentDir
  ]

stringToMsg :: String -> Msg
stringToMsg xs =
  case map toUpper xs of
    "ADDONE"     -> AddOne
    "ADDTWO"     -> AddTwo
    "MINUSONE"   -> MinusOne
    "MINUSTWO"   -> MinusTwo
    "SAYHELLO"   -> SayHello
    "CURRENTDIR" -> CurrentDir
    _            -> NoOp


parseMsg :: String -> Msg
parseMsg msg =
  case parseString msgParser mempty msg of
    Success msg -> msg
    Failure _   -> NoOp
