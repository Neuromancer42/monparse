module DescriptorCombinators
  ( Descr
  , char
  , many
  , orElse
  , primitive
  , nonTerminal
  , anyChar
  , some
  , sepBy
  , newline
  , module GrammarCombinators
  , module ParserCombinators
  ) where

import GrammarCombinators
import ParserCombinators

class Applicative f =>
      Descr f where
  char :: Char -> f ()
  many :: f a -> f [a]
  orElse :: f a -> f a -> f a
  primitive :: String -> Parser a -> f a
  nonTerminal :: String -> f a -> f a

instance Descr Parser where
  char = charP
  many = manyP
  orElse = orElseP
  primitive _ p = p
  nonTerminal _ p = p

instance Descr Grammar where
  char = charG
  many = manyG
  orElse = orElseG
  primitive s _ = primitiveG s
  nonTerminal = nonTerminalG

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

-- some samples
dottedWords
  :: Descr f
  => f [String]
dottedWords = some (many anyChar <* char '.')

newline
  :: Descr f
  => f ()
newline = primitive "newline" (charP '\n')
