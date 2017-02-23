module ParserCombinators
  ( Parser
  , parse
  , anyCharP
  , charP
  , anyCharButP
  , satP
  , bothP
  , sepByP
  , module Control.Applicative.Alternative
  , module Control.Monad.Plus
  ) where

import Control.Applicative.Alternative
import Control.Monad.Plus

newtype Parser a =
  P (String -> [(a, String)])

-- get a parser out of wrapper
runParser :: Parser a -> String -> [(a, String)]
runParser (P p) = p

parse :: Parser a -> String -> [a]
parse p s = selectResult $ runParser p s
  where
    selectResult [] = []
    selectResult ((res, r):ss)
      | null r = res : selectResult ss
      | otherwise = selectResult ss

-- a Parser that always fails
zeroParser :: Parser a
zeroParser = P $ const []

-- a Parser that does nothing but return a single value
pureParser :: a -> Parser a
pureParser x = P $ \inp -> [(x, inp)]

-- instantiation as Functor, Applicative, and Monad
instance Functor Parser where
  fmap f (P p) = P $ map (\(x, inp) -> (f x, inp)) . p

instance Applicative Parser where
  pure = pureParser
  (P pf) <*> (P px) = P $ \inp -> [(f x, inp'') | (f, inp') <- pf inp, (x, inp'') <- px inp']

instance Monad Parser where
  return = pureParser
  (P p) >>= f = P $ \inp -> concat [runParser (f x) inp' | (x, inp') <- p inp]

instance Alternative Parser where
  empty = zeroParser
  (<|>) = orElseP
  many p = ((:) <$> p <*> many p) <|> pure []

instance MonadPlus Parser

-- some primitive parsers
anyCharP :: Parser Char
anyCharP = P f
  where
    f [] = []
    f (x:xs) = [(x, xs)]

charP :: Char -> Parser ()
charP ch = do
  ch' <- anyCharP
  if ch == ch'
    then return ()
    else zeroParser

anyCharButP :: Char -> Parser Char
anyCharButP ch = do
  ch' <- anyCharP
  if ch == ch'
    then zeroParser
    else return ch'

satP :: (a -> Bool) -> Parser a -> Parser a
satP p par = do
  x <- par
  if p x
    then return x
    else zeroParser

orElseP :: Parser a -> Parser a -> Parser a
orElseP (P p) (P q) =
  P $ \inp ->
    if null (p inp)
      then q inp
      else p inp

bothP :: Parser a -> Parser a -> Parser a
bothP (P p) (P q) = P $ \inp -> p inp ++ q inp

sepByP :: Parser a -> Parser () -> Parser [a]
sepByP p q = ((:) <$> p <*> many (q >> p)) <|> return []
