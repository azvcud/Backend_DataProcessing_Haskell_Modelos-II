{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.JSONType
    ( Match(..)
    , Team(..)
    , Score(..)
    , Statistics(..)
    , MVP(..)
    , Highlights(..)
    , Player(..)
    ) where

import Data.Aeson
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
    , highlights_match  :: Highlights 
    } deriving (Show, Generic)

data Team = Team
    { teamId_team   :: Text
    , name_team     :: Text
    , shield_team   :: Text
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
    } deriving (Show, Generic)

data Highlights = Highlights
    { highlightId_highlights    :: Text
    , player_highlights         :: Player
    , moment_highlights         :: Text
    , minute_hightlights        :: Text
    } deriving (Show, Generic)

data Player = Player
    { playerId_player   :: Text
    , name_player       :: Text
    , position_player   :: Text
    , imageUrl_player   :: Text
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
        <*> v .: "highlights"

instance ToJSON Match where
    toJSON (Match mid home away score date stats mvp highlights) =
        object [ "matchId" .= mid
               , "homeTeam" .= home
               , "awayTeam" .= away
               , "score" .= score
               , "date" .= date
               , "statistics" .= stats
               , "mvp" .= mvp
               , "highlights" .= highlights
               ]

instance FromJSON Team where
    parseJSON = withObject "Team" $ \v -> Team
        <$> v .: "teamId"
        <*> v .: "name"
        <*> v .: "shield"

instance ToJSON Team where
    toJSON (Team tid name shield) =
        object [ "teamId" .= tid
               , "name" .= name
               , "shield" .= shield
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
        <*> v .: "yellowCard"
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
               , "yellowCard" .= yellow
               , "redCards" .= red
               , "passes" .= passes
               , "passAcurracy" .= passAccuracy
               ]

instance FromJSON MVP where
    parseJSON = withObject "MVP" $ \v -> MVP
        <$> v .: "playerId"
        <*> v .: "name"
        <*> v .: "stats"

instance ToJSON MVP where
    toJSON (MVP pid name stats) =
        object [ "playerId" .= pid
               , "name" .= name
               , "stats" .= stats
               ]

instance FromJSON Highlights where
    parseJSON = withObject "Highlights" $ \v -> Highlights
        <$> v .: "highlightId"
        <*> v .: "player"
        <*> v .: "moment"
        <*> v .: "minute"

instance ToJSON Highlights where
    toJSON (Highlights hid player moment minute) =
        object [ "highlightId" .= hid
               , "player" .= player
               , "moment" .= moment
               , "minute" .= minute
               ]

instance FromJSON Player where
    parseJSON = withObject "Player" $ \v -> Player
        <$> v .: "playerId"
        <*> v .: "name"
        <*> v .: "position"
        <*> v .: "imageUrl"

instance ToJSON Player where
    toJSON (Player pid name pos img) =
        object [ "playerId" .= pid
               , "name" .= name
               , "position" .= pos
               , "imageUrl" .= img
               ]
