import BNF
import DescriptorCombinators
import System.Environment
import System.Exit
import System.IO

describeSpecialString
  :: Descr f
  => f String
describeSpecialString = flip (:) <$> some letter <*> letter

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> putStr $ ppGrammar "special string" describeSpecialString
    [s] -> putStrLn $ show $ parse describeSpecialString s
