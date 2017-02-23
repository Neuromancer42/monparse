import Data.Char
import ParserCombinators
import System.Environment
import System.Exit
import System.IO

type Identifier = String

type Declaration = (Identifier, String)

type Section = (Identifier, [Declaration])

type INIFile = [Section]

parseINI :: Parser INIFile
parseINI = some parseSection
  where
    parseSection = do
      char '['
      title <- parseIdentifier
      char ']'
      char '\n'
      decls <- many parseLine
      return (title, concat decls)
    parseIdentifier = some parseLetterOrDigit
    parseLetterOrDigit = sat isAlphaNum anyChar
    parseLine = parseDecl <|> parseComment <|> parseEmpty
    parseDecl = do
      i <- parseIdentifier
      _ <- many (char ' ')
      char '='
      _ <- many (char ' ')
      c <- some (anyCharBut '\n')
      char '\n'
      return [(i, c)]
    parseComment = do
      char '#'
      _ <- many (anyCharBut '\n')
      char '\n'
      return []
    parseEmpty = do
      _ <- many (char ' ')
      char '\n'
      return []

main :: IO ()
main = do
  args <- getArgs
  input <-
    case args of
      [] -> getContents
      [fileName] -> readFile fileName
      _ -> hPutStrLn stderr "Too many arguments" >> exitFailure
  case parse parseINI input of
    [] -> do
      hPutStrLn stderr "Failed to parse INI file."
      exitFailure
    x:_ -> print x
