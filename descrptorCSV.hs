import DescriptorCombinators

describeCSV = many describeLine
  where
    describeLine = nonTerminal "line" $ describeCell `sepBy` char ',' <* newline
    describeCell = nonTerminal "cell" $ char '"' *> many (primitive "not-quote" (anyCharButP '"')) <* char '"'
