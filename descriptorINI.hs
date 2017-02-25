import Data.Char
import DescriptorCombinators
import System.Environment
import System.Exit
import System.IO

type Identifier = String

type Declaration = (Identifier, String)

type Section = (Identifier, [Declaration])

type INIFile = [Section]

describeINI
  :: Descr f
  => f INIFile
describeINI = some describeSection
  where
    describeSection =
      nonTerminal "section" $
      pure (,) <* char '[' <*> describeIdentifier <* char ']' <*> (concat <$> many describeLine)
    describeIdentifier = nonTerminal "identifier" $ some (primitive "alphanum" parseLetterOrDigit)
    parseLetterOrDigit = satP isAlphaNum anyCharP
    describeLine = nonTerminal "line" $ describeDecl <|> describeComment <|> describeEmpty
    describeDecl =
      nonTerminal "declaration" $
      (: []) <$> (pure (,) <*> describeIdentifier <* spaces' <* char '=' <* spaces' <*> remainder)
    describeComment = nonTerminal "comment" $ pure [] <* char '#' <* remainder
    describeEmpty = pure [] <* newline
    spaces' = nonTerminal "spaces" $ many (char ' ')
    remainder =
      nonTerminal "line-remainder" $ some (primitive "non-newline" (anyCharButP '\n')) <* newline

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStr $ ppGrammar "ini" describeINI
    [fileName] -> do
      input <- readFile fileName
      case parse describeINI input of
        Nothing -> do
          hPutStrLn stderr "Failed to parse INI file."
          exitFailure
        Just x -> print x
    _ -> hPutStrLn stderr "Too many arguments" >> exitFailure
