{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Main where

import Data.Function ((&))
import Interpret (repl)
import Polysemy
import Polysemy.Reader (runReader)
import Tape (tapeToIO)
import qualified TapeIO

main :: IO ()
main = do
  tape <- TapeIO.initTape
  repl
    & tapeToIO
    & runReader tape
    & runM
