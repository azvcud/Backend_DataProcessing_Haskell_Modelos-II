{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module FetchData.MatchStatisticsAPI
    ( TeamStats(..)
    , MatchStats(..)
    , PlayerID(..)
    , HeadToHead(..)
    , MVPData
    , ScrapeResult
    ) where

import Data.Aeson

import GHC.Generics (Generic)
import Data.Text.Lazy (Text)
import qualified Data.HashMap.Strict as HM

data MatchStats = MatchStats
  { matchId :: Text
  , teams   :: HM.HashMap String TeamStats
  } deriving (Show, Generic)

data PlayerID = PlayerID
  { id :: Text }
  deriving (Show, Generic)

data HeadToHead = HeadToHead
  { team1Name :: String
  , team1Wins :: String
  , team2Name :: String
  , team2Wins :: String
  , draws     :: String
  } deriving (Show, Generic)

data TeamStats = TeamStats
  { possession      :: Double
  , touches        :: Int
  , passes         :: Int
  , shots          :: Int
  , foulsConceded  :: Int
  , tackles        :: Int
  , corners        :: Int
  , clearances     :: Int
  , shotsOnTarget  :: Int
  , yellowCards    :: Int
  , offsides       :: Maybe Int  
  } deriving (Show, Generic)

type MVPData = HM.HashMap String PlayerID
type ScrapeResult = [HeadToHead]

instance FromJSON TeamStats
instance ToJSON TeamStats

instance FromJSON MatchStats
instance ToJSON MatchStats

instance FromJSON PlayerID
instance ToJSON PlayerID

instance FromJSON HeadToHead
instance ToJSON HeadToHead
