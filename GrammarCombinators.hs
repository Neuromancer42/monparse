module GrammarCombinators
  ( Grammar
  , anyCharG
  , charG
  , primitiveG
  , orElseG
  , manyG
  , someG
  , sepByG
  , ppGrammar
  , recNonTerminalG
  , BNF
  ) where

import BNF

newtype Grammar a =
  G (BNF, RHS)

ppGrammar :: String -> Grammar a -> String
ppGrammar m (G (prods, rhs)) = ppBNF $ (m, rhs) : prods

-- primitive combinatos
primitiveG :: String -> Grammar a
primitiveG s = G ([], NonTerminal s)

anyCharG :: Grammar Char
anyCharG = G ([], NonTerminal "char")

charG :: Char -> Grammar ()
charG c = G ([], Terminal [c])

orElseG :: Grammar a -> Grammar a -> Grammar a
orElseG (G (prods1, rhs1)) (G (prods2, rhs2)) = G (mergeProds prods1 prods2, Choice rhs1 rhs2)

-- some instatiation
instance Functor Grammar where
  fmap _ (G (prods, rhs)) = G (prods, rhs)

instance Applicative Grammar where
  pure _ = G ([], Terminal "")
  -- two extra rules to make it lawful
  G (prods1, Terminal "") <*> G (prods2, rhs2) = G (mergeProds prods1 prods2, rhs2)
  G (prods1, rhs1) <*> G (prods2, Terminal "") = G (mergeProds prods1 prods2, rhs1)
  G (prods1, rhs1) <*> G (prods2, rhs2) = G (mergeProds prods1 prods2, Sequence rhs1 rhs2)

-- some derived combinators
manyG :: Grammar a -> Grammar [a]
manyG (G (prods, rhs)) = G (prods, Repetition rhs)

someG :: Grammar a -> Grammar [a]
someG p = pure (:) <*> p <*> manyG p

sepByG :: Grammar a -> Grammar () -> Grammar [a]
sepByG p1 p2 = ((:) <$> p1 <*> manyG (p2 *> p1)) `orElseG` pure []

recNonTerminalG :: String -> (Grammar a -> Grammar a) -> Grammar a
recNonTerminalG name f = G ((name, rhs) : prods, NonTerminal name)
  where
    G (prods, rhs) = f (G ([], NonTerminal name))
