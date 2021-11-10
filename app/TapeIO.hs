{-# OPTIONS_GHC -fplugin=Polysemy.Plugin #-}
module TapeIO where

import Control.Monad.Loops (whileM_)
import Polysemy.Reader
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
import Control.Monad.IO.Class (MonadIO(liftIO))

-- an infinite tape that we can read and write bytes to
data TapeIO = TapeIO
  { tapeNeg :: IORef (IOVector Word8), -- The segment of the tape with negative indices
    tapePos :: IORef (IOVector Word8), -- The segment of the tape with nonnegative indices
    tapePtr :: IORef Int
  }

initTape :: IO TapeIO
initTape = do
  tapeNeg <- newIORef =<< MutVec.replicate 1024 0
  tapePos <- newIORef =<< MutVec.replicate 1024 0
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
  tape <- ask
  i <- pointerPosition
  let (i', vecRef) = trueIndex i tape
      tooShort :: IO Bool
      tooShort = (i' >=) . MutVec.length <$> readIORef vecRef

      -- TODO not sure how to set the values of the newly allocated area to 0
      double :: IO ()
      double = error "infinite tape not yet implemented"
  -- do
  --     vec' <- readIORef vecRef >>= \v -> MutVec.grow v (MutVec.length v) :: IO (IOVector Word8)
  --     writeIORef vecRef vec'

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