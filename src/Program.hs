module Program (
    Program
  , mkProgram
  , mkView
  , View
  , Cmd
  , noCmd
  , mkCmd
) where

import           Control.Monad             (forever)
import           Control.Monad.IO.Class    (liftIO)
import           Control.Monad.Trans.State
import qualified Data.Map                  as M
import           Prelude                   hiding (init)


newtype View msg = View (M.Map String msg)

data Cmd msg =
    None
  | One (IO msg)

data Program msg model =
  Program {
      init   :: (model, Cmd msg)
    , update :: msg -> model -> (model, Cmd msg)
    , view   :: model -> View msg
    }


-- Construct and run Program

mkProgram :: Show model
        => (model, Cmd msg)
        -> (msg -> model -> (model, Cmd msg))
        -> (model -> View msg)
        -> IO ()
mkProgram i u v = runProgram $ Program i u v

mkView :: [(String, msg)] -> View msg
mkView = View . M.fromList


-- Cmds

noCmd :: Cmd msg
noCmd = None

mkCmd :: (a -> msg) -> IO a -> Cmd msg
mkCmd msg action = One $ msg <$> action


-- Handle Program state

runProgram :: Show model => Program msg model -> IO ()
runProgram p = do
  let (m, cmd) = init p
  m' <- execStateT (processCmd p cmd) m
  evalStateT (programState p) m'

programState :: Show model => Program msg model -> StateT model IO ()
programState p = forever $ do
  model  <- get
  parsed <- liftIO $ parseMsg (view p model) <$> getLine
  case parsed of
    Just msg -> do
      let (m', cmd) = update p msg model
      newM  <- liftIO $ execStateT (processCmd p cmd) m'
      liftIO . putStrLn $ "model updated: " ++ show newM
      put newM

    Nothing -> do
      liftIO $ do
        putStrLn "unrecognized input"
        putStrLn $ "model: " ++ show model
      put model


-- Handle Messages and Cmds

parseMsg :: View msg -> String -> Maybe msg
parseMsg (View mappings) input = M.lookup input mappings

processCmd :: Show model => Program msg model -> Cmd msg -> StateT model IO ()
processCmd p None = return ()
processCmd p (One cmd) = do
  model <- get
  msg   <- liftIO cmd
  let (newM, newCmd) = update p msg model
  liftIO . putStrLn $ "processed command, next model: " ++ show newM
  put newM
  processCmd p newCmd
