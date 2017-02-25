module DescriptorCombinators
  ( Descr
  , char
  , many
  , orElse
  , (<|>)
  , primitive
  , nonTerminal
  , recNonTerminal
  , anyChar
  , some
  , sepBy
  , module GrammarCombinators
  , module ParserCombinators
  , newline
  , spaces
  , letter
  , digit
  , nonQuoteOrBackSlash
  ) where

import Data.Char
import GrammarCombinators
import ParserCombinators

class Applicative f =>
      Descr f where
  char :: Char -> f ()
  many :: f a -> f [a]
  orElse :: f a -> f a -> f a
  primitive :: String -> Parser a -> f a
  recNonTerminal :: String -> (f a -> f a) -> f a
  (<|>) :: f a -> f a -> f a
  (<|>) = orElse

instance Descr Parser where
  char = charP
  many = manyP
  orElse = orElseP
  primitive _ p = p
  recNonTerminal _ f =
    let r = f r
    in r

instance Descr Grammar where
  char = charG
  many = manyG
  orElse = orElseG
  primitive s _ = primitiveG s
  recNonTerminal = recNonTerminalG

some
  :: Descr f
  => f a -> f [a]
some d = pure (:) <*> d <*> many d

anyChar
  :: Descr f
  => f Char
anyChar = primitive "char" anyCharP

sepBy
  :: Descr f
  => f a -> f () -> f [a]
sepBy p1 p2 = ((:) <$> p1 <*> many (p2 *> p1)) `orElse` pure []

nonTerminal
  :: Descr f
  => String -> f a -> f a
nonTerminal name descr = recNonTerminal name $ const descr

newline
  :: Descr f
  => f ()
newline = primitive "newline" (charP '\n')

letter
  :: Descr f
  => f Char
letter = primitive "letter" $ satP isAlpha anyCharP

digit
  :: Descr f
  => f Char
digit = primitive "digit" $ satP isDigit anyCharP

spaces
  :: Descr f
  => f [()]
spaces = primitive "spaces" $ many (char ' ' <|> newline)

nonQuoteOrBackSlash
  :: Descr f
  => f Char
nonQuoteOrBackSlash =
  primitive "non-quote-or-backslash" $ satP (\ch -> ch /= '\'' && ch /= '\\') anyCharP
