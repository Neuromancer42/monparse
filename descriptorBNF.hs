import BNF
import DescriptorCombinators
import System.Environment
import System.Exit
import System.IO

describeBNF
  :: Descr f
  => f BNF
describeBNF = some describeProd
  where
    describeProd =
      nonTerminal "production" $
      (,) <$> describeIdent <* spaces <* char '=' <* spaces <*> describeRHS <* char ';' <* spaces
    describeIdent =
      nonTerminal "identifier" $
      (:) <$> letter <*> many (letter <|> digit <|> (char '-' *> pure '-'))
    describeRHS = recNonTerminal "rhs" describeChoice
    describeChoice rhs =
      nonTerminal "choice" $
      mkChoices <$> describeSeq rhs <*> many (spaces *> char '|' *> spaces *> describeSeq rhs) <*
      spaces
    describeSeq rhs =
      nonTerminal "sequence" $
      mkSequences <$> describeAtom rhs <*> many (spaces *> char ',' *> spaces *> describeAtom rhs) <*
      spaces
    describeAtom rhs =
      nonTerminal "atom" $
      describeTerm <|> describeNonTerm <|> describeOpt rhs <|> describeRept rhs <|>
      describeGroup rhs
    describeTerm =
      nonTerminal "terminal" $ Terminal <$> (char '\'' *> many describeQuote <* char '\'' <* spaces)
    describeQuote =
      nonTerminal "quoted-char" $
      nonQuoteOrBackSlash <|> (char '\\' *> char '\\' *> pure '\\') <|>
      (char '\\' *> char '\'' *> pure '\'')
    describeNonTerm = nonTerminal "non-terminal" $ NonTerminal <$> describeIdent <* spaces
    describeOpt rhs = nonTerminal "option" $ Optional <$> braces '[' ']' rhs
    describeRept rhs = nonTerminal "repetition" $ Repetition <$> braces '{' '}' rhs
    describeGroup rhs = nonTerminal "group" $ braces '(' ')' rhs
    braces l r inner = char l *> spaces *> inner <* spaces <* char r <* spaces

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStr $ ppGrammar "bnf" describeBNF
    [fileName] -> do
      input <- readFile fileName
      case parse describeBNF input of
        x:_ -> putStr $ ppBNF x
        [] -> do
          hPutStrLn stderr "Failed to parse INI file."
          exitFailure
    _ -> hPutStrLn stderr "Too many arguments given" >> exitFailure
