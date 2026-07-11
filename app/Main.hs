module Main (main) where

import Universum

import qualified Data.Vector as V
import Codec.Archive.Zip
import Conduit
import Data.Csv
import Exercise
import ExerciseSet
import Fmt
import Graphics.Vega.VegaLite hiding (filter)
import System.Envy

data Paths =
  Paths
  { trackedggExport :: FilePath
  , trackedggVegalite :: FilePath
  }
  deriving (Generic, Show)

instance FromEnv Paths

main :: IO ()
main = do
  result <- runExceptT $ do
    paths <- ExceptT decodeEnv
    liftIO $ fmtLn $ "input: "+|trackedggExport paths|+""
    run paths
  case result of
    Left err         -> fmtLn $ "Error! - "+|err|+""
    Right resultPath -> fmtLn $ "See result at "+|resultPath|+""

type Error = String
type Result = ExceptT Error IO

run :: Paths -> Result FilePath
run Paths { trackedggExport = inputArchivePath, trackedggVegalite = outputPath } = do
  sets <- parseVector inputArchivePath "sets.csv" (type ExerciseSet)
  exercises <- parseVector inputArchivePath "exercises.csv" (type Exercise)
  preacherCurlSets <- allSetsForExercise "Unilateral Machine Preacher Curl" exercises sets
  let dat = exercisesSetsToData preacherCurlSets
      enc = encoding
          . position X [PName "sessionDate", PmType Temporal]
          . position Y [PName "1RM", PmType Quantitative]
      bkg = background "rgba(0, 0, 0, 0.05)"
      vegaLite = toVegaLite [bkg, dat, mark Tick [], enc []]
  liftIO $ toHtmlFile outputPath vegaLite
  pure outputPath

allSetsForExercise :: Text -> Vector Exercise -> Vector ExerciseSet -> Result (Vector ExerciseSet)
allSetsForExercise n es ss = do
  eId <- Exercise.id <$> find (\e -> Exercise.name e == n) es
       & maybeToResult ("exercise '"+|n|+"' not found")
  pure $ V.filter (\s -> exerciseId s == eId) ss

maybeToResult :: Error -> Maybe a -> Result a
maybeToResult err mb = ExceptT . pure $ maybeToRight err mb

parseVector :: FilePath
            -> FilePath
            -> forall a -> FromNamedRecord a
            => Result (Vector a)
parseVector archivePath entryPath _ = do
  selector <- mkEntrySelector entryPath
  entry <- withArchive archivePath (sourceEntry selector sinkLazy)
  hoistEither $ snd <$> decodeByName entry

exercisesSetsToData :: Vector ExerciseSet -> Data
exercisesSetsToData es =
  dataFromRows [] $ concatMap rowFor es
  where
    rowFor e =
      dataRow
        [ ("sessionDate", Str $ sessionDate e)
        , ("exerciseId", Str $ exerciseId e)
        , ("exerciseName", Str $ exerciseName e)
        , ("1RM", Number $ oneRmOConner (weight e) (repetitions e))
        ]
        []

oneRmOConner :: ErrorsBecameZero Double -> Int -> Double
oneRmOConner weight reps =
  let w = unErrorsBecameZero weight
      r = fromIntegral reps
  in
  w * (1 + 0.025 * r)
