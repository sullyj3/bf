{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Tape where

import Data.Word (Word8)
import Polysemy
import Polysemy.Input
import Polysemy.Output
import Control.Monad.Reader (MonadReader)

import Program (Program (..), Statement (..), parseProgram)
import qualified TapeIO
import           TapeIO (TapeIO)
import Polysemy.Reader (Reader)

data Tape m a where
  ReadTape :: Tape m Word8
  WriteTape :: Word8 -> Tape m ()
  PointerPosition :: Tape m Int
  SetPointerPosition :: Int -> Tape m ()

makeSem ''Tape

tapeToIO :: Members [Reader TapeIO, Embed IO] r => InterpreterFor Tape r
tapeToIO = interpret \case
    ReadTape -> TapeIO.readTape
    WriteTape byte -> TapeIO.writeTape byte
    PointerPosition -> TapeIO.pointerPosition
    SetPointerPosition pos -> TapeIO.setPointerPosition pos

modifyCurrentCell :: Member Tape r => (Word8 -> Word8) -> Sem r ()
modifyCurrentCell f = writeTape . f =<< readTape

incCurrentCell :: Member Tape r => Sem r ()
incCurrentCell = modifyCurrentCell (+1)

decCurrentCell :: Member Tape r => Sem r ()
decCurrentCell = modifyCurrentCell (subtract 1)

modifyPointerPosition :: Member Tape r => (Int -> Int) -> Sem r ()
modifyPointerPosition f = setPointerPosition . f =<< pointerPosition