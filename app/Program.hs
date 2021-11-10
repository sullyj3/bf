{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Program (parseProgram, Program(..), Statement(..)) where

import Data.Void
import Text.Megaparsec
import Data.Maybe (fromJust)
import Data.Function ((&))
import Data.Bifunctor
import Text.Megaparsec.Stream (VisualStream)
import Data.Data (Proxy (Proxy))
import Data.List.NonEmpty
import Polysemy.Law (NonEmptyList(NonEmpty))

newtype Program = Program [ Statement ]
  deriving Show

data Statement 
  = SLeft
  | SRight
  | SInc
  | SDec
  | SInput
  | SOutput
  | SLoop [Statement]
  deriving Show

type Parser = Parsec Void String
type ErrBundle = ParseErrorBundle String Void

-- >>> parseProgram ">>+"
-- Right (Program [SRight,SRight,SInc])
-- >>> parseProgram "[+]->>"
-- Right (Program [SLoop [SInc],SDec,SRight,SRight])
-- >>> parseProgram "[]"
-- Right (Program [SLoop []])
parseProgram :: String -> Either ErrBundle Program
parseProgram =
  parse program "interactive"

program :: Parser Program
program = do
  statements <- many statement
  eof
  pure $ Program statements

statement :: Parser Statement
statement = SLeft  <$ single '<' <|>
            SRight <$ single '>' <|>
            SInc   <$ single '+' <|>
            SDec   <$ single '-' <|>
            SInput <$ single ',' <|>
            SOutput <$ single '.' <|>
            SLoop <$> loop

loop :: Parser [Statement]
loop = between
  (single '[')
  (single ']')
  (many statement)
