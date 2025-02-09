{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module FetchData.PremierLeagueTableAPI
    ( PremierLeagueTable(..)
    , PremierLeagueTeam(..)
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

data PremierLeagueTable = PremierLeagueTable
    { premierLeagueTeam_pl :: [PremierLeagueTeam] }
    deriving (Show, Generic)

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