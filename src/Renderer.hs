module Renderer (renderAnnots) where

import           Data.Maybe            (fromMaybe, mapMaybe)
import           System.FilePath.Posix (takeFileName)

import           Config
import           LatexSnippets
import           ParseGroup
import           Text.Diff.Parse.Types

renderAnnots :: Config
             -> Group
             -> Maybe String
             -> [(String, FileDelta)]
             -> Either String String
renderAnnots c g f d =
  case mapM (uncurry (renderAnnot (annotBoxWidth c) (lineAdjustmentFactor c))) d of
    Just conts ->
      let (commentLines, files) = unzip conts
      in Right $
         preamble ++
         concat commentLines ++
         preContent ++
         title (assignment c) (author c) g ++
         fromMaybe "" (feedback <$> f) ++ filesHeader ++ concat files ++ endDoc
    Nothing -> Left "Diff contained binary fiiles"

renderAnnot :: Int -> Float -> String -> FileDelta -> Maybe (String, String)
renderAnnot cpl af src fd@FileDelta {fileDeltaContent = Hunks hunks} =
  Just (comments cpl af fn hunks, contents src fn)
  where
    fn = (takeFileName . fileDeltaSourceFile) fd
renderAnnot _ _ _ FileDelta {fileDeltaContent = Binary} =
  Nothing

comments :: Int -> Float -> String -> [Hunk] -> String
comments col af fn hunks = unlines $ map (hunk col af fn) hunks

hunk :: Int -> Float -> String -> Hunk -> String
hunk cpl af fn (Hunk (Range a _b) _dstRange ls) =
  unlines $ map (comment cpl af fn a) $ mapMaybe filterComment ls

filterComment :: DiffLine -> Maybe String
filterComment (DiffLine Comment txt) =
  if all (\x -> x == '~' || x == ' ' ) txt
    then Nothing
    else Just txt
filterComment _ = Nothing
