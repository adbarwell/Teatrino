module ParserUtils where

import Text.ParserCombinators.Parsec
    ( char,
      oneOf,
      spaces,
      string,
      many,
      GenParser,
      letter,
      alphaNum,
      manyTill,
      anyChar,
      newline,
      (<|>) )
import Control.Monad (void)

comment :: GenParser Char st ()
comment = string "//" >> void (manyTill anyChar newline)

whitespace' :: GenParser Char st ()
whitespace' = void (oneOf " \n\t")

whitespace :: GenParser Char st ()
whitespace = void (many (oneOf " \n\t"))

whiteComments :: GenParser Char st ()
whiteComments = void (many (whitespace' <|> comment))

eos :: GenParser Char st ()
eos = spaces >> char ';' >> whiteComments >> return ()

parens' :: Char -> Char -> GenParser Char st a -> GenParser Char st a
parens' l r p = do
  _ <- char l
  whitespace
  x <- p
  whitespace
  _ <- char r
  return x

parens :: GenParser Char st a -> GenParser Char st a
parens = parens' '(' ')'

braces :: GenParser Char st a -> GenParser Char st a
braces = parens' '{' '}'

reserved :: String -> GenParser Char st ()
reserved x = whitespace >> void (string x) >> whitespace

ident :: GenParser Char st String
ident = do
  l <- letter
  ls <- many alphaNum
  return (l : ls)
