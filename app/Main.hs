module Main (main) where

import Universum

import Codec.Archive.Zip
import Conduit
import Data.Csv
import qualified Data.Vector as Vector
import Graphics.Vega.VegaLite

import ExerciseSet

-- TODO: dynamically pick newest
-- maybe delete others on success
-- maybe cache results
archivePath :: FilePath
archivePath = "D:/Downloads/QuickShare/tracked_export_2026-07-04.zip"

-- TODO: use Reader to pass these
vegaLitePath :: FilePath
vegaLitePath = "D:/My/tracked-gg-graph.html"

main :: IO ()
main = do
  setsSelector <- mkEntrySelector "sets.csv"
  exercisesSelector <- mkEntrySelector "exercises.csv"
  setsEntry <- withArchive archivePath (sourceEntry setsSelector sinkLazy)
  case (decodeByName setsEntry :: Either String (Header, Vector ExerciseSet)) of
    Left errors -> print errors    
    Right (_, exercises) ->
      let dat = exercisesSetsToData exercises
          enc = encoding
              . position X [ PName "sessionDate", PmType Temporal ]
              . position Y [ PName "weight", PmType Quantitative ]
              . color [ MName "reps" ]
          bkg = background "rgba(0, 0, 0, 0.05)"
          vegaLite = toVegaLite [ bkg, dat, mark Tick [], enc [] ]
      in do
        toHtmlFile vegaLitePath vegaLite
        putStrLn vegaLitePath

exercisesSetsToData :: Vector ExerciseSet -> Data
exercisesSetsToData es = 
  dataFromRows [] $ concatMap rowFor es
    where
      rowFor e =
        dataRow
          [ ("sessionId", Str $ sessionId e)
          , ("sessionDate", Str $ sessionDate e)
          , ("exerciseId", Str $ exerciseId e)
          , ("exerciseName", Str $ exerciseName e)
          , ("reps", Number $ fromIntegral (repetitions e))
          , ("weight", Number $ unErrorsBecameZero (weight e))
          , ("rir", maybe NullValue (Number . fromIntegral) (rir e))
          , ("notes", Str $ notes e)
          , ("method", Str $ method e)
          , ("createdAt", Str $ createdAt e)
          , ("updatedAt", Str $ updatedAt e)
          ]
          []
