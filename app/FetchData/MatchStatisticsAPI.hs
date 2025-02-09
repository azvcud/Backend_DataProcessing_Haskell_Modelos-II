{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module FetchData.MatchStatisticsAPI
    ( Statistics(..)
    , MatchStats(..)
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

data MatchStats = MatchStats
  { matchId_matchStats  :: Text
  , teams_matchStats    :: [Statistics]
  } deriving (Show, Generic)

data Statistics = Statistics
  { possesion_matchStats      :: Float
  , touches_matchStats        :: Int
  , passes_matchStats         :: Int
  , shots_matchStats          :: Int
  , foulsConceded_matchStats  :: Int
  , tackles_matchStats        :: Int
  , corners_matchStats        :: Int
  , clearances_matchStats     :: Int
  , shotsOnTarget_matchStats  :: Int
  , yellowCards_matchStats    :: Int
  } deriving (Show, Generic)

instance FromJSON MatchStats where
  parseJSON = withObject "MatchStats" $ \v -> MatchStats
    <$> v .: "matchId"
    <*> v .: "teams"

instance ToJSON MatchStats where
  toJSON (MatchStats matchId teams) =
    object [ "matchId" .= matchId
           , "teams" .= teams ]

instance FromJSON Statistics where
  parseJSON = withObject "Statistics" $ \v -> Statistics
    <$> v .: "possesion"
    <*> v .: "touches"
    <*> v .: "passes"
    <*> v .: "shots"
    <*> v .: "foulsConceded"
    <*> v .: "tackles"
    <*> v .: "corners"
    <*> v .: "clearances"
    <*> v .: "shotsOnTarget"
    <*> v .: "yellowCards"

instance ToJSON Statistics where
  toJSON (Statistics possesion touches passes shots foulsConceded tackles corners clearances shotsOnTarget yellowCards) =
    object [ "possesion" .= possesion
           , "touches" .= touches
           , "passes" .= passes
           , "shots" .= shots
           , "foulsConceded" .= foulsConceded
           , "tackles" .= tackles
           , "corners" .= corners
           , "clearances" .= clearances
           , "shotsOnTarget" .= shotsOnTarget
           , "yellowCards" .= yellowCards ]


