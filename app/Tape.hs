{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

-- An effect representing access to an infinite tape of cells, each cell 
-- containing a byte which can be read from or written to.

module Tape where

import Data.Word (Word8)
import Polysemy

data Tape m a where
  ReadTape :: Tape m Word8
  WriteTape :: Word8 -> Tape m ()
  PointerPosition :: Tape m Int
  SetPointerPosition :: Int -> Tape m ()

makeSem ''Tape

modifyCurrentCell :: Member Tape r => (Word8 -> Word8) -> Sem r ()
modifyCurrentCell f = writeTape . f =<< readTape

modifyPointerPosition :: Member Tape r => (Int -> Int) -> Sem r ()
modifyPointerPosition f = setPointerPosition . f =<< pointerPosition
