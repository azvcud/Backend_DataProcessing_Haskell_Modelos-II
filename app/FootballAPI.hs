{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module FootballAPI ( 
    getFixtures, 
    getMatches,
    getClubs,
    getMatchStatistics,
    getStandings,
    getMVP_ID,
    getScrape,
    getMVP,
    buildUrl
    ) where

import FetchData.ScheduleAPI
    ( Fixture(..)
    , Match(..)
    )

import FetchData.MatchStatisticsAPI 
    ( MatchStats(..)
    , MVPData
    , ScrapeResult
    )

import FetchData.ClubAPI ( Club (..) )
import FetchData.PremierLeagueTableAPI ( PremierLeagueTeam(..) )
import FetchData.MVP_PlayerAPI ( MVPStats(..) )


import Network.Wreq ( get, responseBody )
import Data.Aeson ( decode )
import Control.Lens ( (^.) )
import Data.Text.Lazy (Text, unpack)
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HashMap

--UTILIDADES--
buildUrl :: [Int] -> String
buildUrl matchIds = "http://localhost:3000/scrape?matchIds=" ++ T.unpack (T.intercalate "," (map (T.pack . show) matchIds))

--DONE--
getFixtures :: IO (Maybe [Fixture])
getFixtures = do
    response <- get "http://localhost:3000/fixtures"
    return $ decode (response ^. responseBody)

--DONE--
getMatches :: IO (Maybe [Match])
getMatches = do
    response <- get "http://localhost:3000/match-results"
    return $ decode (response ^. responseBody)

--DONE--
getClubs :: IO (Maybe (HashMap.HashMap Int Club))
getClubs = do
    response <- get "http://localhost:3000/badges"
    return $ decode (response ^. responseBody)

--DONE--
-- Definimos la función GET similar a la que has mostrado
getMatchStatistics :: Text -> IO (Maybe MatchStats)
getMatchStatistics match_id = do
  response <- get ("http://localhost:3000/match-stats/" ++ unpack match_id)
  return $ decode (response ^. responseBody)

--DONE--
getStandings :: IO (Maybe [PremierLeagueTeam])
getStandings = do
    response <- get "http://localhost:3000/standings"
    return $ decode (response ^. responseBody)

--DONE--
getMVP_ID :: Text -> IO (Maybe MVPData)
getMVP_ID pollId = do
    response <- get $ ("http://localhost:3000/poll-mvp/" ++ unpack pollId)
    return $ decode (response ^. responseBody)

--DONE--
getScrape :: Text -> IO (Maybe ScrapeResult)
getScrape matchIds = do
    response <- get ("http://localhost:3000/scrape?matchIds=" ++ unpack matchIds)
    return $ decode (response ^. responseBody)  -- Decodificar la respuesta como ScrapeResult

--DONE--
getMVP :: Text -> Int -> IO (Maybe MVPStats)
getMVP playerId match_id = do
    response <- get ("http://localhost:3000/mvp-stats/" ++ unpack playerId ++ "/" ++ show match_id)
    return $ decode (response ^. responseBody)