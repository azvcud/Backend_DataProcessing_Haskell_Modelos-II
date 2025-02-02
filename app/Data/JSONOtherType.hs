{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.JSONOtherType
    ( Standing(..)
    , UpcomingMatch(..)
    , MatchAnalysis(..)
    , KeyBattle(..)
    , ClubCompetition(..)
    , HeatMap(..)
    ) where

import Data.Aeson
import GHC.Generics (Generic)
import Data.Text.Lazy (Text)

data Standing = Standing 
    { position_standing         :: Int
    , team_standing             :: Text
    , played_standing           :: Int
    , points_standing           :: Int
    , shield_standing           :: Text
    , form_standing             :: [Text]
    , goalDifference_standing   :: Int
    } deriving (Show, Generic)

data UpcomingMatch = UpcomingMatch
    { id_upcomingMatch              :: Int
    , homeTeam_upcomingMatch        :: Text
    , awayTeam_upcomingMatch        :: Text
    , date_upcomingMatch            :: Text
    , time_upcomingMatch            :: Text
    , competition_upcomingMatch     :: Text
    , venue_upcomingMatch           :: Text
    , homeShield_upcomingMatch      :: Text
    , awayShield_upcomingMatch      :: Text
    , ticketStatus_upcomingMatch    :: Text
    , broadcast_upcomingMatch       :: Text
    } deriving (Show, Generic)

data MatchAnalysis = MatchAnalysis
    { matchId_matchAnalysis             :: Text
    , title_matchAnalysis               :: Text
    , summary_matchAnaylsis             :: Text
    , keyPoints_matchAnalysis           :: [Text]
    , tacticalFormations_matchAnalysis  :: ClubCompetition
    , keyBattles_matchAnalysis          :: [KeyBattle]
    , heatmaps_matchAnalysis            :: HeatMap
    } deriving (Show, Generic)

data KeyBattle = KeyBattle
    { area_keyBattle        :: Text
    , players_keyBattle     :: ClubCompetition
    , analysis_keyBattle    :: Text
    } deriving (Show, Generic)

data ClubCompetition = ClubCompetition
    { home_ClubCompetition :: Text
    , away_ClubCompetition :: Text
    } deriving (Show, Generic)

data HeatMap = HeatMap
    { posession_heatMap :: ClubCompetition
    } deriving (Show, Generic)

instance FromJSON Standing where
    parseJSON = withObject "Standing" $ \v -> Standing
        <$> v .: "position"
        <*> v .: "team"
        <*> v .: "played"
        <*> v .: "points"
        <*> v .: "shield"
        <*> v .: "form"
        <*> v .: "goalDifference"

instance ToJSON Standing where
    toJSON (Standing pos team played points shield form gd) =
        object [ "position" .= pos
               , "team" .= team
               , "played" .= played
               , "points" .= points
               , "shield" .= shield
               , "form" .= form
               , "goalDifference" .= gd
               ]

instance FromJSON UpcomingMatch where
    parseJSON = withObject "UpcomingMatch" $ \v -> UpcomingMatch
        <$> v .: "id"
        <*> v .: "homeTeam"
        <*> v .: "awayTeam"
        <*> v .: "date"
        <*> v .: "time"
        <*> v .: "competition"
        <*> v .: "venue"
        <*> v .: "homeShield"
        <*> v .: "awayShield"
        <*> v .: "ticketStatus"
        <*> v .: "broadcast"

instance ToJSON UpcomingMatch where
    toJSON (UpcomingMatch upId homeTeam awayTeam date time competition venue homeShield awayShield ticketStatus broadcast) =
        object [ "id" .= upId
               , "homeTeam" .= homeTeam
               , "awayTeam" .= awayTeam
               , "date" .= date
               , "time" .= time
               , "competition" .= competition
               , "venue" .= venue
               , "homeShield" .= homeShield
               , "awayShield" .= awayShield
               , "ticketStatus" .= ticketStatus
               , "broadcast" .= broadcast
               ]

instance FromJSON MatchAnalysis where
    parseJSON = withObject "MatchAnalysis" $ \v -> MatchAnalysis
        <$> v .: "matchId"
        <*> v .: "title"
        <*> v .: "summary"
        <*> v .: "keyPoints"
        <*> v .: "tacticalFormations"
        <*> v .: "keyBattles"
        <*> v .: "heatmaps"

instance ToJSON MatchAnalysis where
    toJSON (MatchAnalysis matchId title summary keyPoints tacticalFormations keyBattles heatmaps) =
        object [ "matchId" .= matchId
               , "title" .= title
               , "summary" .= summary
               , "keyPoints" .= keyPoints
               , "tacticalFormations" .= tacticalFormations
               , "keyBattles" .= keyBattles
               , "heatmaps" .= heatmaps
               ]

instance FromJSON KeyBattle where
    parseJSON = withObject "KeyBattle" $ \v -> KeyBattle
        <$> v .: "area"
        <*> v .: "players"
        <*> v .: "analysis"

instance ToJSON KeyBattle where
    toJSON (KeyBattle area players analysis) =
        object [ "area" .= area
               , "players" .= players
               , "analysis" .= analysis
               ]

instance FromJSON ClubCompetition where
    parseJSON = withObject "ClubCompetition" $ \v -> ClubCompetition
        <$> v .: "home"
        <*> v .: "away"

instance ToJSON ClubCompetition where
    toJSON (ClubCompetition home away) =
        object [ "home" .= home
               , "away" .= away
               ]

instance FromJSON HeatMap where
    parseJSON = withObject "HeatMap" $ \v -> HeatMap
        <$> v .: "posession"

instance ToJSON HeatMap where
    toJSON (HeatMap poss) =
        object [ "posession" .= poss]