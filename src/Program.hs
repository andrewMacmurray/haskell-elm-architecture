module Program (
    Program
  , mkProgram
  , mkView
  , View
  , Cmd
  , noCmd
  , mkCmd
) where

import           Control.Monad              (forever)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Control.Monad.Trans.State
import qualified Data.Map                   as M
import           Prelude                    hiding (init)

data Program msg model =
  Program {
      init   :: (model, Cmd msg)
    , update :: msg -> model -> (model, Cmd msg)
    , view   :: model -> View msg
    }

data Cmd msg =
    None
  | One (IO msg)

newtype View msg = View (M.Map String msg)


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

type ProgramState msg model =
  ReaderT (Program msg model) (StateT model IO) ()


runProgram :: Show model => Program msg model -> IO ()
runProgram p = do
  let (m, cmd) = init p
      read_ x  = runReaderT x p
  m' <- execStateT (read_ $ processCmd cmd) m
  evalStateT (read_ programState) m'

programState :: Show model => ProgramState msg model
programState = forever $ do
  prog   <- ask
  model  <- lift get
  parsed <- liftIO $ parseMsg (view prog model) <$> getLine
  case parsed of
    Just msg -> do
      let (m', cmd) = update prog msg model
      lift $ put m'
      processCmd cmd
      liftIO . putStrLn $ "model updated: " ++ show m'

    Nothing -> do
      liftIO $ do
        putStrLn "unrecognized input"
        putStrLn $ "model: " ++ show model
      lift $ put model


-- Handle Messages and Cmds

parseMsg :: View msg -> String -> Maybe msg
parseMsg (View mappings) input = M.lookup input mappings

processCmd :: Show model =>  Cmd msg -> ProgramState msg model
processCmd None = return ()
processCmd (One cmd) = do
  prog  <- ask
  model <- lift get
  msg   <- liftIO cmd
  let (newM, newCmd) = update prog msg model
  liftIO . putStrLn $ "processed command, next model: " ++ show newM
  lift $ put newM
  processCmd newCmd
