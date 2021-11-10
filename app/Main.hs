{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Main where

import Control.Monad (forever)
import Control.Monad.IO.Class
import Interpret (repl)
import Polysemy
import Polysemy.Input
import Polysemy.Output
import Polysemy.Reader (runReader, Reader)
import Program (Program (Program), Statement)
import Tape (Tape, tapeToIO)
import qualified TapeIO
import           TapeIO (TapeIO)
import Data.Function ((&))

main :: IO ()
main = do
  -- tape :: TapeIO
  tape <- TapeIO.initTape
  -- repl :: Sem '[Tape, Embed IO] ()
  repl 
  -- tapeToIO :: Members [Reader TapeIO, Embed IO] r => InterpreterFor Tape r
    & tapeToIO
    & runReader tape
    & runM
