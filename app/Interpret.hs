{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Interpret where

import Control.Monad.Loops (whileM_)
import Flow
import Polysemy
import Program (Program (..), Statement (..), parseProgram)
import Tape (Tape)
import qualified Tape
import Text.Megaparsec (errorBundlePretty)
import qualified Data.Text as Text

runProgram :: Members [Tape, Embed IO] r => Program -> Sem r ()
runProgram (Program statements) = mapM_ runStatement statements

runStatement :: Members [Tape, Embed IO] r => Statement -> Sem r ()
runStatement = \case
  SLeft -> Tape.modifyPointerPosition (+ 1)
  SRight -> Tape.modifyPointerPosition (subtract 1)
  SInc -> Tape.modifyCurrentCell (+ 1)
  SDec -> Tape.modifyCurrentCell (subtract 1)
  SInput -> do
    byte <- fix \retry -> do
      line <- prompt "byte> "
      whenNothing (readMaybe $ Text.unpack line) do
        putTextLn $ "couldn't read \"" <> line <> "\" as a byte, try again."
        retry

    Tape.writeTape byte
  SOutput -> liftIO . print =<< Tape.readTape
  SLoop statements -> do
    whileM_ ((/= 0) <$> Tape.readTape) (mapM_ runStatement statements)

repl :: Members [Tape, Embed IO] r => Sem r ()
repl = forever do
  prompt "BF> " >>= parseProgram
    .> either
      (liftIO . putStrLn . errorBundlePretty)
      runProgram

prompt :: MonadIO m => Text -> m Text
prompt s = do
  putText s
  hFlush stdout
  getLine
