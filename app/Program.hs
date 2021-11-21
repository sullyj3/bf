{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Program (parseProgram, Program (..), Statement (..)) where

import Prelude hiding (many)
import Text.Megaparsec

newtype Program = Program [Statement]
  deriving (Show)

data Statement
  = SLeft -- <
  | SRight -- >
  | SInc -- +
  | SDec -- -
  | SInput -- ,
  | SOutput -- .
  | SLoop [Statement] -- [ ]
  deriving (Show)

type Parser = Parsec Void Text

type ErrBundle = ParseErrorBundle Text Void

-- >>> parseProgram ">>+"
-- Right (Program [SRight,SRight,SInc])
-- >>> parseProgram "[+]->>"
-- Right (Program [SLoop [SInc],SDec,SRight,SRight])
-- >>> parseProgram "[[][+[]]]"
-- Right (Program [SLoop [SLoop [],SLoop [SInc,SLoop []]]])
-- $setup
-- >>> import Data.Either (isLeft)
-- >>> isLeft $ parseProgram "["
-- True
parseProgram :: Text -> Either ErrBundle Program
parseProgram =
  parse program "interactive"

-- Todo: allow comments and whitespace
program :: Parser Program
program = Program <$> many statement <* eof

statement :: Parser Statement
statement =
  SLeft <$ single '<'
    <|> SRight <$ single '>'
    <|> SInc <$ single '+'
    <|> SDec <$ single '-'
    <|> SInput <$ single ','
    <|> SOutput <$ single '.'
    <|> SLoop <$> loop

loop :: Parser [Statement]
loop =
  between
    (single '[')
    (single ']')
    (many statement)
