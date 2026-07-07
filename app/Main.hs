module Main (main) where

import Universum

import Codec.Archive.Zip
import Conduit
import Data.Csv
import Data.Vector qualified as Vector
import ExerciseSet
import Graphics.Vega.VegaLite

import System.Envy

data Paths =
  Paths
  { trackedggExport :: FilePath
  , trackedggVegalite :: FilePath
  }
  deriving (Generic, Show)

instance FromEnv Paths

main :: IO ()
main = decodeEnv >>= either print (\ps -> print ps >> run ps)

run :: Paths -> IO ()
run Paths { trackedggExport = inputArchivePath, trackedggVegalite = outputPath } = do
  setsSelector <- mkEntrySelector "sets.csv"
  exercisesSelector <- mkEntrySelector "exercises.csv"
  setsEntry <- withArchive inputArchivePath (sourceEntry setsSelector sinkLazy)
  case (decodeByName setsEntry :: Either String (Header, Vector ExerciseSet)) of
    Left errors -> print errors
    Right (_, exercises) ->
      let dat = exercisesSetsToData exercises
          enc = encoding . position X [PName "sessionDate", PmType Temporal]
                         . position Y [PName "weight", PmType Quantitative]
                         . color [MName "reps"]
          bkg = background "rgba(0, 0, 0, 0.05)"
          vegaLite = toVegaLite [bkg, dat, mark Tick [], enc []]
      in do
        toHtmlFile outputPath vegaLite


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
