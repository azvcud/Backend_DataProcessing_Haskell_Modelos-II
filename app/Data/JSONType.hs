{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.JSONType
    ( Match(..)
    , Team(..)
    , Score(..)
    , Standing(..)
    ) where

import Data.Aeson
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

data Standing = Standing
    { position :: Int
    , team :: Text
    , played :: Int
    , points :: Int
    , shield_S :: Text
    } deriving (Show, Generic)

-- Instancias de FromJSON y ToJSON para los tipos de datos
instance FromJSON Team
instance ToJSON Team

instance FromJSON Score
instance ToJSON Score

instance FromJSON Match
instance ToJSON Match

instance FromJSON Standing where
    parseJSON = withObject "Standing" $ \v -> Standing
        <$> v .: "position"
        <*> v .: "team"
        <*> v .: "played"
        <*> v .: "points"
        <*> v .: "shield"

instance ToJSON Standing where
    toJSON (Standing position_JS team_JS played_JS points_JS shield_JS) =
        object [ "position" .= position_JS
               , "team" .= team_JS
               , "played" .= played_JS
               , "points" .= points_JS
               , "shield" .= shield_JS  
               ]

