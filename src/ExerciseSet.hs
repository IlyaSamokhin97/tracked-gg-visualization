{-# LANGUAGE DeriveGeneric #-}

module ExerciseSet (module ExerciseSet) where

import Universum

import Data.Csv

data ExerciseSet =
  ExerciseSet
  { id :: !Text
  , sessionId :: !Text
  , sessionDate :: !Text
  , exerciseId :: !Text
  , exerciseName :: !Text
  , repetitions :: !Int
  , weight :: !(ErrorsBecameZero Double)
  , rir :: !(Maybe Int)
  , notes :: !Text
  , method :: !Text
  , secondaryRepetitions :: !Text
  , secondaryWeight :: !Text
  , secondaryRir :: !Text
  , secondaryNotes :: !Text
  , createdAt :: !Text
  , updatedAt :: !Text
  } deriving (Generic, Show)

instance FromNamedRecord ExerciseSet
instance DefaultOrdered ExerciseSet

-- | Wrapper for parsing numbers, which maps any errors to value 0.
-- TODO: think of a nicer way to do this - to not leak parsing business to the data type.
newtype ErrorsBecameZero a =
  ErrorsBecameZero a
  deriving (Show)

unErrorsBecameZero :: ErrorsBecameZero a -> a
unErrorsBecameZero (ErrorsBecameZero x) = x

instance (Num a, FromField a) =>
         FromField (ErrorsBecameZero a) where
  parseField = pure . ErrorsBecameZero . fromRight 0 . runParser . parseField
