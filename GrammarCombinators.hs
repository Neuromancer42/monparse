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
  , nonTerminalG
  ) where

import Data.List

-- RHS is short for RightHand Side (in a ENBF expression)
data RHS
  = Terminal String
  | NonTerminal String
  | Choice RHS
           RHS
  | Sequence RHS
             RHS
  | Optional RHS
  | Repetition RHS
  deriving (Show, Eq)

-- convert a list of RHS into a single RHS
mkChoices :: RHS -> [RHS] -> RHS
mkChoices = foldl Choice

mkSequences :: RHS -> [RHS] -> RHS
mkSequences = foldl Sequence

-- pretty-print a RHS
ppRHS :: RHS -> String
ppRHS = go 0
  where
    go _ (Terminal s) = surround "'" "'" $ concatMap quote s
    go _ (NonTerminal s) = s
    go a (Choice x1 x2) = p a 1 $ go 1 x1 ++ " | " ++ go 1 x2
    go a (Sequence x1 x2) = p a 2 $ go 2 x1 ++ ", " ++ go 2 x2
    go _ (Optional x) = surround "[" "]" $ go 0 x
    go _ (Repetition x) = surround "{" "}" $ go 0 x
    surround c1 c2 x = c1 ++ x ++ c2
    p a n
      | a > n = surround "(" ")"
      | otherwise = id
    quote '\'' = "\\'"
    quote '\\' = "\\\\"
    quote c = [c]

type Production = (String, RHS)

type BNF = [Production]

ppBNF :: BNF -> String
ppBNF = unlines . map (\(i, rhs) -> i ++ " = " ++ ppRHS rhs ++ ";")

newtype Grammar a =
  G (BNF, RHS)

ppGrammar :: String -> Grammar a -> String
ppGrammar m (G (prods, rhs)) = ppBNF $ (m, rhs):prods

-- primitive combinatos
primitiveG :: String -> Grammar a
primitiveG s = G ([], NonTerminal s)

anyCharG :: Grammar Char
anyCharG = G ([], NonTerminal "char")

charG :: Char -> Grammar ()
charG c = G ([], Terminal [c])

orElseG :: Grammar a -> Grammar a -> Grammar a
orElseG (G (prods1, rhs1)) (G (prods2, rhs2)) = G (mergeProds prods1 prods2, Choice rhs1 rhs2)

mergeProds :: [Production] -> [Production] -> [Production]
mergeProds prods1 prods2 = nub $ prods1 ++ prods2

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

nonTerminalG :: String -> Grammar a -> Grammar a
nonTerminalG name (G (prods, rhs))
  = G ((name, rhs):prods, NonTerminal name)
