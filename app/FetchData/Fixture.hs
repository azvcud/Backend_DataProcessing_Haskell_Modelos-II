{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module FetchData.Fixture
    ( Fixture(..)
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