{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

module Tape where

import Data.Word (Word8)
import Polysemy
import Polysemy.Reader (Reader)
import TapeIO (TapeIO)
import qualified TapeIO

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

modifyPointerPosition :: Member Tape r => (Int -> Int) -> Sem r ()
modifyPointerPosition f = setPointerPosition . f =<< pointerPosition