{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module FetchData.ScheduleAPI
    ( Fixture(..),
      Match(..)
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

data Fixture = Fixture
    { homeTeam_fixture  :: Text
    , awayTeam_fixture  :: Text
    , matchTime_fixture :: Text
    } deriving (Show, Generic)

data Match = Match
    { date_match        :: Text
    , homeTeam_match    :: Text
    , awayTeam_match    :: Text
    , score_match       :: Text
    , matchId_match     :: Text
    , mvpId_match       :: Text
    } deriving (Show, Generic)

instance FromJSON Fixture where
    parseJSON = withObject "Fixture" $ \v -> Fixture
        <$> v .: "homeTeam"
        <*> v .: "awayTeam"
        <*> v .: "matchTime"

instance ToJSON Fixture where
    toJSON (Fixture home away match) =
        object [ "homeTeam" .= home
               , "awayTeam" .= away
               , "matchTime" .= match
               ]

instance FromJSON Match where
    parseJSON = withObject "Match" $ \v -> Match
        <$> v .: "date"
        <*> v .: "homeTeam"
        <*> v .: "awayTeam"
        <*> v .: "score"
        <*> v .: "matchId"
        <*> v .: "mvpId"

instance ToJSON Match where
    toJSON (Match date home away score mid mvp) =
        object [ "date" .= date
               , "homeTeam" .= home
               , "awayTeam" .= away
               , "score" .= score
               , "matchId" .= mid
               , "mvpId" .= mvp
               ]