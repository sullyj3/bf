{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Main where

import Interpret (repl)
import Polysemy
import Polysemy.Reader (runReader)
import Tape (tapeToIO)
import qualified TapeIO
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
