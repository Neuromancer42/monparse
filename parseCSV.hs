import ParserCombinators
import System.Environment
import System.Exit
import System.IO

parseCSV :: Parser [[String]]
parseCSV = many parseLine
  where
    parseLine = parseCell `sepBy` char ',' <* char '\n'
    parseCell = do
      char '"'
      content <- many (anyCharBut '"')
      char '"'
      return content

main :: IO ()
main = do
  args <- getArgs
  input <-
    case args of
      [] -> getContents
      [fileName] -> readFile fileName
      _ -> hPutStrLn stderr "Too many arguments" >> exitFailure
  case parse parseCSV input of
    [] -> do
      hPutStrLn stderr "Failed to parse INI file."
      exitFailure
    x:_ -> print x
