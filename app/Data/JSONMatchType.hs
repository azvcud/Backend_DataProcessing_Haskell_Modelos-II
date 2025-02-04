{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.JSONMatchType
    ( Match(..)
    , Team(..)
    , Score(..)
    , Statistics(..)
    , MVP(..)
    , HeadToHead(..)
    ) where

import Data.Aeson
    ( (.:),
      withObject,
      object,
      FromJSON(parseJSON),
      KeyValue((.=)),
      ToJSON(toJSON) 
    )
    
import GHC.Generics (Generic)
import Data.Text.Lazy (Text)

-- Definición de los tipos de datos
data Match = Match
    { matchId_match     :: Text
    , homeTeam_match    :: Team
    , awayTeam_match    :: Team
    , score_match       :: Score
    , date_match        :: Text
    , statistics_match  :: Statistics
    , mvp_match         :: MVP
    , headToHead_match  :: HeadToHead
    } deriving (Show, Generic)

data Team = Team
    { teamId_team   :: Text
    , name_team     :: Text
    , shield_team   :: Text
    , wins_team     :: Int
    , draws_team    :: Int
    } deriving (Show, Generic)

data Score = Score
    { home_score    :: Int
    , away_score    :: Int
    } deriving (Show, Generic)

data Statistics = Statistics
    { possesion_statistics      :: Score
    , shots_statistics          :: Score
    , shotsOnTarget_statistics  :: Score
    , corners_statistics        :: Score
    , fouls_statistics          :: Score
    , yellowCard_statistics     :: Score
    , redCards_statistics       :: Score
    , passes_statistics         :: Score
    , passAcurracy_statistics   :: Score
    } deriving (Show, Generic)

data MVP = MVP
    { playerId_MVP  :: Text
    , name_MVP      :: Text
    , stats_MVP     :: Text
    , imageUrl_MVP  :: Text
    } deriving (Show, Generic)

data HeadToHead = HeadToHead
    { homeTeam_headtohead   :: Team
    , awayTeam_headtohead   :: Team
    } deriving (Show, Generic)

-- Instancias de FromJSON y ToJSON para los tipos de datos
instance FromJSON Match where
    parseJSON = withObject "Match" $ \v -> Match
        <$> v .: "matchId"
        <*> v .: "homeTeam"
        <*> v .: "awayTeam"
        <*> v .: "score"
        <*> v .: "date"
        <*> v .: "statistics"
        <*> v .: "mvp"
        <*> v .: "headToHead"

instance ToJSON Match where
    toJSON (Match mid home away score date stats mvp headhead) =
        object [ "matchId" .= mid
               , "homeTeam" .= home
               , "awayTeam" .= away
               , "score" .= score
               , "date" .= date
               , "statistics" .= stats
               , "mvp" .= mvp
               , "headToHead" .= headhead
               ]

instance FromJSON Team where
    parseJSON = withObject "Team" $ \v -> Team
        <$> v .: "teamId"
        <*> v .: "name"
        <*> v .: "shield"
        <*> v .: "wins"
        <*> v .: "draws"

instance ToJSON Team where
    toJSON (Team tid name shield win draw) =
        object [ "teamId" .= tid
               , "name" .= name
               , "shield" .= shield
               , "wins" .= win
               , "draws" .= draw
               ]

instance FromJSON Score where
    parseJSON = withObject "Score" $ \v -> Score
        <$> v .: "home"
        <*> v .: "away"

instance ToJSON Score where
    toJSON (Score home away) =
        object [ "home" .= home
               , "away" .= away
               ]

instance FromJSON Statistics where
    parseJSON = withObject "Statistics" $ \v -> Statistics
        <$> v .: "possesion"
        <*> v .: "shots"
        <*> v .: "shotsOnTarget"
        <*> v .: "corners"
        <*> v .: "fouls"
        <*> v .: "yellowCards"
        <*> v .: "redCards"
        <*> v .: "passes"
        <*> v .: "passAcurracy"

instance ToJSON Statistics where
    toJSON (Statistics pos shots shotsOnTarget corners fouls yellow red passes passAccuracy) =
        object [ "possesion" .= pos
               , "shots" .= shots
               , "shotsOnTarget" .= shotsOnTarget
               , "corners" .= corners
               , "fouls" .= fouls
               , "yellowCards" .= yellow
               , "redCards" .= red
               , "passes" .= passes
               , "passAcurracy" .= passAccuracy
               ]

instance FromJSON MVP where
    parseJSON = withObject "MVP" $ \v -> MVP
        <$> v .: "playerId"
        <*> v .: "name"
        <*> v .: "stats"
        <*> v .: "imageUrl"

instance ToJSON MVP where
    toJSON (MVP pid name stats url) =
        object [ "playerId" .= pid
               , "name" .= name
               , "stats" .= stats
               , "imageUrl" .= url
               ]

instance ToJSON HeadToHead where
    toJSON (HeadToHead homeTeam awayTeam) = object
        [ "homeTeam" .= homeTeam
        , "awayTeam" .= awayTeam
        ]

instance FromJSON HeadToHead where
    parseJSON = withObject "HeadToHead" $ \v -> HeadToHead
        <$> v .: "homeTeam"
        <*> v .: "awayTeam"