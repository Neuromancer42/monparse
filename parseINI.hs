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
      charP '['
      title <- parseIdentifier
      charP ']'
      charP '\n'
      decls <- many parseLine
      return (title, concat decls)
    parseIdentifier = some parseLetterOrDigit
    parseLetterOrDigit = satP isAlphaNum anyCharP
    parseLine = parseDecl <|> parseComment <|> parseEmpty
    parseDecl = do
      i <- parseIdentifier
      _ <- many (charP ' ')
      charP '='
      _ <- many (charP ' ')
      c <- some (anyCharButP '\n')
      charP '\n'
      return [(i, c)]
    parseComment = do
      charP '#'
      _ <- many (anyCharButP '\n')
      charP '\n'
      return []
    parseEmpty = do
      _ <- many (charP ' ')
      charP '\n'
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
