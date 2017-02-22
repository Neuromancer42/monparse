newtype Parser a =
  P (String -> [(a, String)])

-- consume nothing and returns a single value
result :: a -> Parser a
result v = P $ \inp -> [(v, inp)]

-- always fail
zero :: Parser a
zero = P $ const []

-- consume one single character
item :: Parser Char
item =
  P $ \inp ->
    case inp of
      [] -> []
      (x:xs) -> [(x, xs)]

main :: IO ()
main = undefined
