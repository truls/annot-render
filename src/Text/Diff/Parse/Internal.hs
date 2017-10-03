module Text.Diff.Parse.Internal
    ( parseDiff
    , diff
    , annotation
    , annotatedLine
    , hunk
    , fileDelta
    , fileDeltaHeader
    ) where

import           Text.Diff.Parse.Types

import           Text.Parsec
import           Text.Parsec.String

parseDiff :: String -> String -> Either ParseError FileDeltas
parseDiff = parse diff

diff :: Parser FileDeltas
diff = many1 fileDelta <* eof

fileDelta :: Parser FileDelta
fileDelta = do
  (source, dest) <- fileDeltaHeader
  content <- try binary <|> hunks
  return $ FileDelta source dest content

fileDeltaHeader :: Parser (String, String)
fileDeltaHeader = do
  source <- string "--- " *> path <* takeLine
  dest <- string "+++ " *> path <* takeLine
  return (source, dest)

takeLine :: Parser String
takeLine = manyTill anyChar endOfLine

path :: Parser String
path = manyTill anyChar tab

hunks :: Parser Content
hunks = Hunks <$> many hunk

hunk :: Parser Hunk
hunk =
  Hunk <$> (string "@@ -" *> range) <*>
  (string " +" *> range <* string " @@" <* takeLine) <*>
  many annotatedLine

binary :: Parser Content
binary =
  string "Binary files " >> takeLine >> pure Binary

range :: Parser Range
range = Range <$> number <*> option 1 (string "," *> number)

number :: Parser Int
number = (read :: String -> Int) <$> many1 (digit <|> char '-')

annotatedLine :: Parser DiffLine
annotatedLine =
  DiffLine <$> annotation <*> takeLine <*
  option "" (string "\\ No newline at end of file" <* endOfLine)

annotation :: Parser Annotation
annotation = (char '+' >> return Added)
         <|> (char '-' >> return Removed)
         <|> (char ' ' >> return Context)
         <|> (char '#' >> return Comment)
