module BNF
( RHS (..)
, mkChoices
, mkSequences
, mergeProds
, ppRHS
, ppBNF
, Production
, BNF
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
    go :: Integer -> RHS -> String
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

mergeProds :: [Production] -> [Production] -> [Production]
mergeProds prods1 prods2 = nub $ prods1 ++ prods2

type BNF = [Production]

ppBNF :: BNF -> String
ppBNF = unlines . map (\(i, rhs) -> i ++ " = " ++ ppRHS rhs ++ ";")
