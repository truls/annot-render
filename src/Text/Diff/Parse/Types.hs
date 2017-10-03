module Text.Diff.Parse.Types where

data Annotation
  = Added
  | Removed
  | Context
  | Comment
  deriving (Show, Eq)

data DiffLine = DiffLine
  { lineAnnotation :: Annotation
  , lineContent    :: String
  } deriving (Show, Eq)

data Range = Range
  { rangeStartingLineNumber :: Int
  , rangeNumberOfLines      :: Int
  } deriving (Show, Eq)

data Hunk = Hunk
  { hunkSourceRange :: Range
  , hunkDestRange   :: Range
  , hunkLines       :: [DiffLine]
  } deriving (Show, Eq)

data Content = Binary | Hunks [Hunk] deriving (Show, Eq)

data FileDelta = FileDelta
  { fileDeltaSourceFile :: String
  , fileDeltaDestFile   :: String
  , fileDeltaContent    :: Content
  } deriving (Show, Eq)

type FileDeltas = [FileDelta]
