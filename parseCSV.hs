import ParserCombinators

parseCSV = many parseLine
  where
    parseLine = parseCell `sepBy` char ',' <* char '\n'
    parseCell = do
        char '"'
        content <- many (anyCharBut '"')
        char '"'
        return content
