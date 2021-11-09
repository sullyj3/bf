{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Main where

import Control.Monad.Loops (whileM_)
import Data.IORef
  ( IORef,
    modifyIORef,
    newIORef,
    readIORef,
    writeIORef,
  )
import qualified Data.Vector as Vec
import Data.Vector.Mutable (IOVector)
import qualified Data.Vector.Mutable as MutVec
import Data.Void (Void, vacuous)
import Data.Word (Word8)
import System.IO (BufferMode (LineBuffering), hFlush, hSetBuffering, stdout)

data Instruction
  = ILeft
  | IRight
  | IInc
  | IDec
  | IOutput
  | IInput
  | IJumpRightIfZero
  | IJumpLeftIfNonZero
  deriving (Show)

data Tape = Tape
  { tapeNeg :: IORef (IOVector Word8), -- The segment of the tape with negative indices
    tapePos :: IORef (IOVector Word8)  -- The segment of the tape with nonnegative indices
  }

initTape :: IO Tape
initTape = do
  tapeNeg <- newIORef =<< MutVec.replicate 1024 0
  tapePos <- newIORef =<< MutVec.replicate 1024 0
  pure $ Tape {..}

parse :: String -> Maybe [Instruction]
parse = traverse parseInstruction

parseInstruction :: Char -> Maybe Instruction
parseInstruction = \case
  '<' -> Just ILeft
  '>' -> Just IRight
  '+' -> Just IInc
  '-' -> Just IDec
  '.' -> Just IOutput
  ',' -> Just IInput
  '[' -> Just IJumpRightIfZero
  ']' -> Just IJumpLeftIfNonZero
  _ -> Nothing

data ProgState = ProgState {tape :: Tape, ptr :: IORef Int}

initState :: IO ProgState
initState = do
  tape <- initTape
  ptr <- newIORef 0
  pure $ ProgState {..}

step :: ProgState -> Instruction -> IO ()
step progstate@ProgState {..} instruction = do
  ptr' <- readIORef ptr
  case instruction of
    ILeft -> modifyIORef ptr (subtract 1)
    IRight -> modifyIORef ptr (+ 1)
    IInc -> inc tape ptr'
    IDec -> dec tape ptr'
    IOutput -> output tape ptr'
    IInput -> undefined
    IJumpRightIfZero -> undefined
    IJumpLeftIfNonZero -> undefined

inc :: Tape -> Int -> IO ()
inc t = modifyTape t (+ 1)

dec :: Tape -> Int -> IO ()
dec t = modifyTape t (subtract 1)

output :: Tape -> Int -> IO ()
output tape i = do
  let (vecRef, i') = indexTape tape i
  vec <- readIORef vecRef
  print =<< MutVec.read vec i'

data TapeIx = IxNeg Int | IxPos Int
  deriving (Show)

toTapeIx :: Int -> TapeIx
toTapeIx i = case compare i 0 of
  LT -> IxNeg (abs i - 1) -- -1 will be 0 in tapeNeg, -2 will be 1, etc.
  _ -> IxPos i

indexTape :: Tape -> Int -> (IORef (IOVector Word8), Int)
indexTape Tape {..} i = case toTapeIx i of
  IxNeg i' -> (tapeNeg, i')
  IxPos i' -> (tapePos, i')

modifyTape :: Tape -> (Word8 -> Word8) -> Int -> IO ()
modifyTape tape f i = do
  let (vecRef, i') = indexTape tape i
      tooShort :: IO Bool
      tooShort = (i' >=) . MutVec.length <$> readIORef vecRef

      -- TODO this probably won't work - not sure how to set the values of the newly allocated area to 0
      double :: IO ()
      double = do
        vec' <- readIORef vecRef >>= \v -> MutVec.grow v (MutVec.length v) :: IO (IOVector Word8)
        writeIORef vecRef vec'

  whileM_ tooShort double
  readIORef vecRef >>= \v -> MutVec.modify v f i'

run :: ProgState -> [Instruction] -> IO ()
run s = mapM_ (step s)

showState :: ProgState -> IO String
showState ProgState {..} = do
  let Tape {..} = tape
  neg <- Vec.freeze =<< readIORef tapeNeg
  pos <- Vec.freeze =<< readIORef tapePos
  p <- readIORef ptr
  pure $ unlines [show neg, show pos, show p]

main :: IO ()
main = do
  s <- initState
  let loop :: IO Void
      loop = do
        putStr "BF> "
        hFlush stdout
        l <- getLine
        case parse l of
          Just program -> do
            run s program
            loop
          Nothing -> do
            putStrLn "invalid program"
            loop
  vacuous loop
