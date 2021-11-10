{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
module Program (parseProgram, Program(..), Statement(..)) where

import Text.Megaparsec
import Data.Maybe (fromJust)
import Data.Function ((&))
import Data.Bifunctor
import Text.Megaparsec.Stream (VisualStream)
import Data.Data (Proxy (Proxy))
import Data.List.NonEmpty
import Polysemy.Law (NonEmptyList(NonEmpty))

data Lexeme
  = LLeft
  | LRight
  | LInc
  | LDec
  | LOutput
  | LInput
  | LJumpRightIfZero
  | LJumpLeftIfNonZero
  deriving (Show, Eq, Ord)


-- >>> let input = "[]"
-- >>> lexProgram input
-- Right [LJumpRightIfZero,LJumpLeftIfNonZero]
lexProgram :: String -> Either LexError [Lexeme]
lexProgram = traverse lexInstruction

newtype LexError = UnexpectedChar Char
  deriving (Show, Eq, Ord)

lexInstruction :: Char -> Either LexError Lexeme
lexInstruction = \case
  '<' -> Right LLeft
  '>' -> Right LRight
  '+' -> Right LInc
  '-' -> Right LDec
  '.' -> Right LOutput
  ',' -> Right LInput
  '[' -> Right LJumpRightIfZero
  ']' -> Right LJumpLeftIfNonZero
  c -> Left $ UnexpectedChar c

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

type Parser = Parsec () [Lexeme]
type ErrBundle = ParseErrorBundle [Lexeme] ()

type ProgParseError = Either LexError ErrBundle

-- >>> parseProgram ">>+"
-- Right (Program [SRight,SRight,SInc])
-- >>> parseProgram "[+]->>"
-- Right (Program [SLoop [SInc],SDec,SRight,SRight])
-- >>> parseProgram "[]"
-- Right (Program [SLoop []])
parseProgram :: String -> Either ProgParseError Program
parseProgram s = do
  lexemes <- lexProgram s             & first Left
  parse program "interactive" lexemes & first Right

program :: Parser Program
program = do
  statements <- many statement
  pure $ Program statements

statement :: Parser Statement
statement = SLeft  <$ single LLeft <|>
            SRight <$ single LRight <|>
            SInc   <$ single LInc <|>
            SDec   <$ single LDec <|>
            SInput <$ single LInput <|>
            SOutput <$ single LOutput <|>
            SLoop <$> loop

loop :: Parser [Statement]
loop = between
  (single LJumpRightIfZero)
  (single LJumpLeftIfNonZero)
  (many statement)
