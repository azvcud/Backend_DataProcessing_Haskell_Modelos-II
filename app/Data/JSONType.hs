{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.JSONType
    ( Match(..)
    , Team(..)
    , Score(..)
    ) where

import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Data.Text.Lazy (Text)

-- Definición de los tipos de datos
data Match = Match
    { matchId   :: Int
    , homeTeam  :: Team
    , awayTeam  :: Team
    , score     :: Score
    , date      :: Text
    } deriving (Show, Generic)

data Team = Team
  { name :: Text
  , shield :: Text
  } deriving (Show, Generic)

data Score = Score
  { home :: Int
  , away :: Int
  } deriving (Show, Generic)

-- Instancias de FromJSON y ToJSON para los tipos de datos
instance FromJSON Team
instance ToJSON Team

instance FromJSON Score
instance ToJSON Score

instance FromJSON Match
instance ToJSON Match

