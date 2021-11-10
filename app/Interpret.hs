{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
module Interpret where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Polysemy
import Polysemy.Input
import Polysemy.Output
import Program (Program (..), Statement (..), parseProgram)
import System.IO (hFlush, stdout)
import qualified Tape
import           Tape (Tape)
import Text.Read (readMaybe)
import Control.Monad (forever)
import Flow
import Text.Megaparsec (errorBundlePretty)

runProgram :: Members [Tape, Embed IO] r => Program -> Sem r ()
runProgram (Program statements) = mapM_ runStatement statements

runStatement :: Members [Tape, Embed IO] r => Statement -> Sem r ()
runStatement = \case
  SLeft -> Tape.modifyPointerPosition (+ 1)
  SRight -> Tape.modifyPointerPosition (subtract 1)
  SInc -> Tape.modifyCurrentCell (+ 1)
  SDec -> Tape.modifyCurrentCell (subtract 1)
  SInput ->
    let loop = do
          l <- liftIO
            do
              putStr "byte> "
              hFlush stdout
              getLine
          case readMaybe l of
            Just b -> Tape.writeTape b
            Nothing -> do
              liftIO $ putStrLn $ "couldn't read " ++ l ++ " as a byte, try again."
              loop
     in loop
  SOutput -> liftIO . print =<< Tape.readTape
  SLoop statements -> do
    error "loops not yet implemented"

repl :: Members [Tape, Embed IO] r => Sem r ()
repl = forever do
  liftIO do putStr "BF> "
            hFlush stdout
  liftIO getLine >>= parseProgram .> \case
    Right program -> do
      runProgram program
    Left err -> do
      liftIO $ putStrLn (errorBundlePretty err)