{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module FetchData.PremierLeagueTableAPI
    ( PremierLeagueTeam(..)
    , TeamName(..)
    , Overall(..)
    , Form(..)
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

data PremierLeagueTeam = PremierLeagueTeam
    { team_premier      :: TeamName
    , position_premier  :: Int
    , overall_premier   :: Overall
    , form_premier      :: [Form]
    } deriving (Show, Generic)

data TeamName = TeamName
    { name_team :: Text }
    deriving (Show, Generic)

data Overall = Overall
    { played_overall            :: Int
    , points_overall            :: Int
    , goalsDifference_overall   :: Int
    } deriving (Show, Generic)

data Form = Form
    { outcome_form :: Text }
    deriving (Show, Generic)


-- Instancia FromJSON para PremierLeagueTeam

instance FromJSON PremierLeagueTeam where
    parseJSON = withObject "PremierLeagueTeam" $ \v -> PremierLeagueTeam
        <$> v .: "team"
        <*> v .: "position"
        <*> v .: "overall"
        <*> v .: "form"

-- Instancia ToJSON para PremierLeagueTeam

instance ToJSON PremierLeagueTeam where
    toJSON (PremierLeagueTeam team position overall form) =
        object [ "team" .= team
               , "position" .= position
               , "overall" .= overall
               , "form" .= form ]

-- Instancia FromJSON para TeamName

instance FromJSON TeamName where
    parseJSON = withObject "TeamName" $ \v -> TeamName
        <$> v .: "name"

-- Instancia ToJSON para TeamName

instance ToJSON TeamName where
    toJSON (TeamName name) = object [ "name" .= name ]

-- Instancia FromJSON para Overall

instance FromJSON Overall where
    parseJSON = withObject "Overall" $ \v -> Overall
        <$> v .: "played"
        <*> v .: "points"
        <*> v .: "goalsDifference"

-- Instancia ToJSON para Overall

instance ToJSON Overall where
    toJSON (Overall played points goalsDifference) =
        object [ "played" .= played
               , "points" .= points
               , "goalsDifference" .= goalsDifference ]

-- Instancia FromJSON para Form

instance FromJSON Form where
    parseJSON = withObject "Form" $ \v -> Form
        <$> v .: "outcome"

-- Instancia ToJSON para Form

instance ToJSON Form where
    toJSON (Form outcome) = object [ "outcome" .= outcome ]