import GrammarCombinators

grammarCSV :: Grammar [[String]]
grammarCSV = manyG grammarLine
  where
    grammarLine = nonTerminalG "line" $ grammarCell `sepByG` charG ',' <* primitiveG "newline"
    grammarCell = nonTerminalG "cell" $ charG '"' *> manyG (primitiveG "not-quote") <* charG '"'
