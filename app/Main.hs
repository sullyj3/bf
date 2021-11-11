{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Main where

import Data.Function ((&))
import Interpret (repl)
import Polysemy
import Polysemy.Reader (runReader)
import qualified TapeIO

main :: IO ()
main = do
  tape <- TapeIO.initTape
  repl
    & TapeIO.tapeToIO
    & runReader tape
    & runM
