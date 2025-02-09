{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module FootballAPI ( 
    getFixtures, 
    getMatches,
    getClubs,
    getMatchStatistics
    ) where

import FetchData.ScheduleAPI
    ( Fixture(..)
    , Match(..)
    )

import FetchData.ClubAPI ( Club (..) )

import FetchData.MatchStatisticsAPI ( MatchStats(..) )


import Network.Wreq ( get, responseBody )
import Data.Aeson ( decode )
import Control.Lens ( (^.) )

getFixtures :: IO (Maybe [Fixture])
getFixtures = do
    response <- get "http://localhost:3000/fixtures"
    return $ decode (response ^. responseBody)

getMatches :: IO (Maybe [Match])
getMatches = do
    response <- get "http://localhost:3000/match-results"
    return $ decode (response ^. responseBody)

getClubs :: IO (Maybe [Club])
getClubs = do
    response <- get "http://localhost:3000/clubs"
    return $ decode (response ^. responseBody)

getMatchStatistics :: Int -> IO (Maybe [MatchStats])
getMatchStatistics matchId = do
    response <- get ("http://localhost:3000/match-stats/" ++ show matchId)
    return $ decode (response ^. responseBody)
