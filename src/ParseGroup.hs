module ParseGroup
  ( Group
  , parseGroup
  ) where

import           Text.Parsec
import           Text.Parsec.String

type Group = [String]

parseGroup :: String -> String -> Either ParseError Group
parseGroup = parse names

names :: Parser Group
names = many1 kuname <* eof

kuname :: Parser String
kuname = (++) <$> count 3 letter <*> count 3 digit <* spaces
