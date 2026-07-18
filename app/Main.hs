module Main (main) where

import Universum

import qualified Data.Aeson as J
import qualified Data.Aeson.KeyMap as JKeyMap
import qualified Data.Vector as V
import qualified Fmt
import Codec.Archive.Zip
import Conduit
import Data.Bifunctor
import Data.Csv
import Exercise
import ExerciseSet
import Fmt hiding (Buildable)
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
    paths <- ExceptT . fmap (first DecodeEnvFailed) $ decodeEnv
    -- TODO: output inputs explicitly, e.g. "env vars: ENV_VAR = foo"
    liftIO $ fmtLn $ "input: "+|trackedggExport paths|+""
    vizualizeToHtml
      (ArchivedCsvs (trackedggExport paths))
      (Html       (trackedggVegalite paths))
  case result of
    Left err                -> fmtLn $ "Error! - "+|err|+""
    Right (Html resultPath) -> fmtLn $ "See result at file://"+|resultPath|+""

data Error =
    ExerciseNotFound Text
  | DecodeEnvFailed String
  | DecodeCsvFailed String

instance Fmt.Buildable Error where
  build = \case
    ExerciseNotFound n -> "exercise "+|n|+" not found"
    DecodeEnvFailed e -> "decodeEnv failed with error: '"+|e|+"'"
    DecodeCsvFailed e -> "decode csv failed with error: '"+|e|+"'"

type Result = ExceptT Error IO

newtype ArchivedCsvs = ArchivedCsvs FilePath
newtype Html = Html FilePath

vizualizeToHtml :: ArchivedCsvs -> Html -> Result Html
vizualizeToHtml (ArchivedCsvs inputArchivePath) (Html outputPath) = do
  sets <- parseVector inputArchivePath "sets.csv" (type ExerciseSet)
  exercises <- parseVector inputArchivePath "exercises.csv" (type Exercise)
  setsByExercise <- traverse (\x -> (x,) <$> allSetsForExercise exercises sets x)
    -- TODO: move this out using a Reader?
    [ "Unilateral Machine Preacher Curl"
    , "Isolateral Frontal Plane Pulldown"
    ]
  liftIO $
    toHtmlFileWith htmlOptions outputPath $
    toVegaLite [vConcat (toVLSpec <$> setsByExercise)]
  pure $ Html outputPath
  where
    toVLSpec :: (Text, Vector ExerciseSet) -> VLSpec
    toVLSpec (exerciseName, sets) = asSpec
      [ title exerciseName []
      , bkg
      , exercisesSetsToData sets
      , mark Line []
      , enc []
      , width 800
      , height 800
      ]

    enc = encoding
        . position X [PName "sessionDate", PmType Temporal]
        . position Y [PName "1RM", PmType Quantitative]

    bkg = background "rgba(0, 0, 0, 0.05)"

    htmlOptions :: Maybe J.Value
    htmlOptions = Just . J.Object . JKeyMap.fromList $
      [ ("scaleFactor", J.Number 1)
      ]

allSetsForExercise :: Vector Exercise -> Vector ExerciseSet -> Text -> Result (Vector ExerciseSet)
allSetsForExercise es sets n = do
  exId <- Exercise.id <$> find (withName n) es & maybeToResult (ExerciseNotFound n)
  pure $ V.filter (normalWithId exId) sets
  where
    withName nm e      = Exercise.name e == nm
    normalWithId eId s = exerciseId s == eId
                      && method s == ExerciseSet.Normal

maybeToResult :: Error -> Maybe a -> Result a
maybeToResult err mb = ExceptT . pure $ maybeToRight err mb

parseVector :: FilePath
            -> FilePath
            -> forall a -> FromNamedRecord a
            => Result (Vector a)
parseVector archivePath entryPath _ = do
  selector <- mkEntrySelector entryPath
  entry <- withArchive archivePath (sourceEntry selector sinkLazy)
  hoistEither $ bimap DecodeCsvFailed snd $ decodeByName entry

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
