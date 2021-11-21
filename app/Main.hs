{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Main where

import Interpret (repl)
import Polysemy
import Polysemy.Reader (runReader)
import qualified TapeIO

import Prelude hiding (runReader)

main :: IO ()
main = do
  tape <- TapeIO.initTape
  repl
    & TapeIO.tapeToIO
    & runReader tape
    & runM
