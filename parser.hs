main :: IO ()
main = undefined

newtype Parser a =
  P (String -> [(a, String)])

-- get a parser out of wrapper
runParser :: Parser a -> String -> [(a, String)]
runParser (P p) = p

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

-- some primitive parsers
anyChar :: Parser Char
anyChar = P f
  where
    f [] = []
    f (x:xs) = [(x, xs)]

char :: Char -> Parser ()
char ch = do
  ch' <- anyChar
  if ch == ch'
    then return ()
    else zeroParser

anyCharBut :: Char -> Parser Char
anyCharBut ch = do
  ch' <- anyChar
  if ch == ch'
    then zeroParser
    else return ch'

orElse :: Parser a -> Parser a -> Parser a
orElse (P p) (P q) =
  P $ \inp ->
    if null (p inp)
      then q inp
      else p inp

both :: Parser a -> Parser a -> Parser a
both (P p) (P q) = P $ \inp -> p inp ++ q inp

many :: Parser a -> Parser [a]
many p = do
  ((:) <$> p <*> many p) `orElse` return []

sepBy :: Parser a -> Parser () -> Parser [a]
sepBy p q = ((:) <$> p <*> many (q >> p)) `orElse` return []
