import Data.Char
import DescriptorCombinators
import System.Environment
import System.Exit
import System.IO

describeCSV :: Descr f => f [[String]]
describeCSV = many describeLine
  where
    describeLine = nonTerminal "line" $ describeCell `sepBy` char ',' <* newline
    describeCell = nonTerminal "cell" $ char '"' *> many (primitive "not-quote" (anyCharButP '"')) <* char '"'

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStr $ ppGrammar "csv" describeCSV
    [fileName] -> do
      input <- readFile fileName
      case parse describeCSV input of
        [] -> do
          hPutStrLn stderr "Failed to parse csv file."
          exitFailure
        x:_ -> print x
    _ -> hPutStrLn stderr "Too many arguments" >> exitFailure
