{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Interpret where

import Control.Monad (forever, when)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Loops (untilJust)
import Data.Maybe (isNothing)
import Flow
import Polysemy
import Program (Program (..), Statement (..), parseProgram)
import System.IO (hFlush, stdout)
import Tape (Tape)
import qualified Tape
import Text.Megaparsec (errorBundlePretty)
import Text.Read (readMaybe)

runProgram :: Members [Tape, Embed IO] r => Program -> Sem r ()
runProgram (Program statements) = mapM_ runStatement statements

runStatement :: Members [Tape, Embed IO] r => Statement -> Sem r ()
runStatement = \case
  SLeft -> Tape.modifyPointerPosition (+ 1)
  SRight -> Tape.modifyPointerPosition (subtract 1)
  SInc -> Tape.modifyCurrentCell (+ 1)
  SDec -> Tape.modifyCurrentCell (subtract 1)
  SInput -> do
    byte <- untilJust do
      line <- prompt "byte> "
      let mbyte = readMaybe line
      when
        (isNothing mbyte)
        ( liftIO . putStrLn $
            "couldn't read \"" ++ line ++ "\" as a byte, try again."
        )
      pure mbyte
    Tape.writeTape byte
  SOutput -> liftIO . print =<< Tape.readTape
  SLoop _statements -> do
    error "loops not yet implemented"

repl :: Members [Tape, Embed IO] r => Sem r ()
repl = forever do
  prompt "BF> " >>= parseProgram
    .> either
      (liftIO . putStrLn . errorBundlePretty)
      runProgram

prompt :: MonadIO m => String -> m String
prompt s = liftIO do
  putStr s
  hFlush stdout
  getLine