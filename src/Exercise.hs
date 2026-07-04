{-# LANGUAGE DeriveGeneric #-}

module Exercise (module Exercise) where

import Universum

import Data.Csv

data Exercise =
  Exercise
  { id :: !Text
  , name :: !Text
  } deriving (Generic, Show)

instance FromNamedRecord Exercise
instance DefaultOrdered Exercise
