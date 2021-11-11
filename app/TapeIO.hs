{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}

-- An implementation of the `Tape` effect from Tape.hs in terms of IORefs and
-- IOVectors

module TapeIO where

import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Loops (whileM_)
import Data.IORef
  ( IORef,
    newIORef,
    readIORef,
    writeIORef,
  )
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as MutVec
import Data.Word (Word8)
import Polysemy
import Polysemy.Reader (Reader, ask)
import Tape (Tape (..))

tapeToIO :: Members [Reader TapeIO, Embed IO] r => InterpreterFor Tape r
tapeToIO = interpret \case
  ReadTape -> readTape
  WriteTape byte -> writeTape byte
  PointerPosition -> pointerPosition
  SetPointerPosition pos -> setPointerPosition pos

-- an infinite tape that we can read and write bytes to
data TapeIO = TapeIO
  { tapeNeg :: IORef (IOVector Word8), -- The segment of the tape with negative indices
    tapePos :: IORef (IOVector Word8), -- The segment of the tape with nonnegative indices
    tapePtr :: IORef Int
  }

initTape :: IO TapeIO
initTape = do
  tapeNeg <- newIORef =<< MutVec.replicate 8 0
  tapePos <- newIORef =<< MutVec.replicate 8 0
  tapePtr <- newIORef 0
  pure $ TapeIO {..}

-- newtype TapeIOM a = TapeIOM {runTapeIOM :: ReaderT TapeIO IO a}
--   deriving (Functor, Applicative, Monad, MonadReader TapeIO, MonadIO)

-- since we use two vectors, one for positive indices and one for negative indices,
-- this allows us to view the appropriate vector at the index we want to access
trueIndex :: Int -> TapeIO -> (Int, IORef (IOVector Word8))
trueIndex i TapeIO {..} = case compare i 0 of
  LT -> (abs i - 1, tapeNeg)
  _ -> (i, tapePos)

currTrueIndex :: Members [Reader TapeIO, Embed IO] r => Sem r (Int, IORef (IOVector Word8))
currTrueIndex = do
  tape <- ask
  i <- pointerPosition
  pure $ trueIndex i tape

-- if the pointer is outside the currently allocated tape, we need to allocate
-- more tape in order to be able to read from and write to the current position
ensureTapeLongEnough :: Members [Reader TapeIO, Embed IO] r => Sem r ()
ensureTapeLongEnough = do
  (i, vecRef) <- currTrueIndex
  let tooShort :: IO Bool
      tooShort = (i >=) . MutVec.length <$> readIORef vecRef

      double :: IO ()
      double = do
        old <- readIORef vecRef
        let len = MutVec.length old
        new <- MutVec.grow old len
        let newHalf = MutVec.drop len new
        MutVec.set newHalf 0
        writeIORef vecRef new

  liftIO $ whileM_ tooShort double

readTape :: Members [Reader TapeIO, Embed IO] r => Sem r Word8
readTape = do
  ensureTapeLongEnough
  (i, vecRef) <- currTrueIndex
  liftIO do
    vec <- readIORef vecRef
    MutVec.read vec i

writeTape :: Members [Reader TapeIO, Embed IO] r => Word8 -> Sem r ()
writeTape byte = do
  ensureTapeLongEnough
  (i, vecRef) <- currTrueIndex
  liftIO do
    vec <- readIORef vecRef
    MutVec.write vec i byte

pointerPosition :: Members [Reader TapeIO, Embed IO] r => Sem r Int
pointerPosition = do
  TapeIO {tapePtr} <- ask
  liftIO $ readIORef tapePtr

setPointerPosition :: Members [Reader TapeIO, Embed IO] r => Int -> Sem r ()
setPointerPosition i = do
  TapeIO {tapePtr} <- ask
  liftIO $ writeIORef tapePtr i
