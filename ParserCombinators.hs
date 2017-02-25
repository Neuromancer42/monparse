module ParserCombinators
  ( Parser
  , parse
  , anyCharP
  , charP
  , anyCharButP
  , satP
  , sepByP
  , manyP
  , someP
  , orElseP
  ) where

newtype Parser a =
  P (String -> Maybe (a, String))

-- get a parser out of wrapper
runParser :: Parser a -> String -> Maybe (a, String)
runParser (P p) = p

parse :: Parser a -> String -> Maybe a
parse p s = selectResult $ runParser p s
  where
    selectResult Nothing = Nothing
    selectResult (Just (result, "")) = Just result
    selectResult _ = Nothing

-- a Parser that always fails
zeroParser :: Parser a
zeroParser = P $ const Nothing

-- a Parser that does nothing but return a single value
pureParser :: a -> Parser a
pureParser x = P $ \inp -> Just (x, inp)

-- instantiation as Functor, Applicative, and Monad
instance Functor Parser where
  fmap f (P p) = P $ fmap (\(x, inp) -> (f x, inp)) . p

instance Applicative Parser where
  pure = pureParser
  (P pf) <*> (P px) =
    P $ \inp -> do
      (f, inp') <- pf inp
      (x, inp'') <- px inp'
      return (f x, inp'')

instance Monad Parser where
  return = pureParser
  (P p) >>= f =
    P $ \inp -> do
      (x, inp') <- p inp
      runParser (f x) inp'

-- some primitive parsers
anyCharP :: Parser Char
anyCharP = P f
  where
    f [] = Nothing
    f (x:xs) = Just (x, xs)

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

manyP :: Parser a -> Parser [a]
manyP p = ((:) <$> p <*> manyP p) `orElseP` pure []

someP :: Parser a -> Parser [a]
someP p = (:) <$> p <*> manyP p

sepByP :: Parser a -> Parser () -> Parser [a]
sepByP p q = ((:) <$> p <*> manyP (q >> p)) `orElseP` return []
