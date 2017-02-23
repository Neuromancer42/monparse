import Data.Char
import DescriptorCombinators

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
    describeLine = nonTerminal "line" $ describeDecl `orElse` describeComment `orElse` describeEmpty
    describeDecl =
      nonTerminal "declaration" $
      (: []) <$> (pure (,) <*> describeIdentifier <* spaces <* char '=' <* spaces <*> remainder)
    describeComment = nonTerminal "comment" $ pure [] <* char '#' <* remainder
    describeEmpty = pure [] <* newline
    spaces = nonTerminal "spaces" $ many (char ' ')
    remainder = nonTerminal "line-remainder" $ some (primitive "non-newline" (anyCharButP '\n')) <* newline
