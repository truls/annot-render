{-# LANGUAGE LambdaCase #-}

module Main where

import           Control.Exception     (throwIO)
import           Control.Monad         (foldM)
import qualified Data.ByteString.Char8 as BS
import           Data.Semigroup        ((<>))
import           Data.Tuple            (uncurry)
import           Options.Applicative
import           System.Exit
import           System.FilePath.Posix (takeFileName)
import qualified Text.Parsec           as P (ParseError)

import           Config
import           ParseGroup
import           Renderer
import           Text.Diff.Parse
import           Text.Diff.Parse.Types

data Options = Options
  { diffFile  :: [String]
  , optFile   :: String
  , groupFile :: String
  , feedback  :: Maybe String
  }

renderFile :: Options -> IO ()
renderFile opts = do
  config <-
    parseConfig <$> BS.readFile (optFile opts) >>= \case
      Nothing -> throwIO $ userError "Could not parse config file"
      Just c -> return c
  groups <-
    parseGroup (takeFileName (groupFile opts)) <$> readFile (groupFile opts) >>= \case
      Left e -> throwIO $ userError (show e)
      Right s -> return s
  content <-
    mapM
      (\s -> readFile s >>= (\r -> return (takeFileName s, r)))
      (diffFile opts)
  parsed <-
    case foldParsed $ map (uncurry parseDiff) content of
      Left e  -> throwIO $ userError (show e)
      Right s -> return s
  srcFiles <- mapM (readFile . fileDeltaSourceFile) parsed
  feedbackF <-
    case feedback opts of
      Just f  -> Just <$> readFile f
      Nothing -> pure Nothing
  case renderAnnots config groups feedbackF (zip srcFiles parsed) of
    Left e  -> throwIO $ userError (show e)
    Right s -> putStrLn s

foldParsed :: [Either P.ParseError FileDeltas] -> Either P.ParseError FileDeltas
foldParsed = foldM (fmap . (++)) []

optParser :: Parser Options
optParser =
  Options <$>
  some
    (strOption
       (long "diff" <> metavar "DIFF" <> short 'd' <>
        help "Diff file to parse (Can be specified multiple times)")) <*>
  strOption
    (long "config" <> short 'c' <>
     help "A YAML file containing general assignment-wide options") <*>
  strOption
    (long "group" <> short 'g' <>
     help "File containing student group authoring this assignment") <*>
  optional
    (strOption
       (long "feedback" <> short 'f' <> help "File containing general feedback"))

main :: IO ()
main = do
  o <- execParser opts
  renderFile o
  exitSuccess
  where
    opts =
      info
        (optParser <**> helper)
        (fullDesc <>
         progDesc
           "Generate student feedback reports by rendering source code annotations nicely" <>
         header "annot-render: Student feedback generator")
