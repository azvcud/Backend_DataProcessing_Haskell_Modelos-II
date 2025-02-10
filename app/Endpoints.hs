{-# LANGUAGE OverloadedStrings #-}

module Endpoints (app) where

import Network.HTTP.Types.Status (notFound404)
import Web.Scotty ( get, json, param, queryParam, pathParam, raw, setHeader, status, ScottyM )
import Data.Text.Lazy (Text)
import Data.List (find)
import Control.Monad.IO.Class (liftIO)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)

import FootballAPI
    ( getFixtures,
      getMatches,
      getClubs,
      getMatchStatistics,
      getStandings,
      getMVP_ID,
      getScrape,
      getMVP ) 

import Data.JSONMatchType 
    ( Match(..)
    , Team(..)
    , Score(..)
    , Statistics(..)
    , MVP(..)
    , HeadToHead(..)
    )

import Data.JSONOtherType
    ( Standing(..)
    , UpcomingMatch(..)
    , MatchAnalysis(..)
    , KeyBattle(..)
    , ClubCompetition(..)
    , HeatMap(..)
    )

-- JSON de Ejemplo
exampleMatches :: [Match]
exampleMatches =
  [ Match
      "1"
      (Team "ARS" "Arsenal" "https://resources.premierleague.com/premierleague/badges/t3.svg" 10 5)
      (Team "TOT" "Tottenham" "https://resources.premierleague.com/premierleague/badges/t6.svg" 8 5)
      (Score 3 2)
      "2024-01-20"
      (Statistics
        (Score 55 45)
        (Score 18 12)
        (Score 8 5)
        (Score 7 4)
        (Score 9 14)
        (Score 2 3)
        (Score 0 0)
        (Score 578 432)
        (Score 89 83)
      )
      (MVP "BS7" "Bukayo Saka" "2 Goals, 1 Assist, 7 Key Passes, 91% Pass Accuracy" "https://resources.premierleague.com/premierleague/photos/players/250x250/p223340.png")
      (HeadToHead 
        (Team "ARS" "Arsenal" "https://resources.premierleague.com/premierleague/badges/t3.svg" 10 5) 
        (Team "TOT" "Tottenham" "https://resources.premierleague.com/premierleague/badges/t6.svg" 8 5)
      )
  , Match
      "2"
      (Team "LIV" "Liverpool" "https://resources.premierleague.com/premierleague/badges/t14.svg" 12 6)
      (Team "MCI" "Manchester City" "https://resources.premierleague.com/premierleague/badges/t43.svg" 11 6)
      (Score 2 2)
      "2024-01-21"
      (Statistics
        (Score 48 52)
        (Score 14 15)
        (Score 6 7)
        (Score 8 6)
        (Score 11 8)
        (Score 2 1)
        (Score 0 0)
        (Score 523 612)
        (Score 86 91)
      )
      (MVP "MS11" "Mohamed Salah" "1 Goal, 1 Assist, 6 Key Passes, 4 Successful Dribbles" "https://resources.premierleague.com/premierleague/photos/players/250x250/p118748.png")
      (HeadToHead
        (Team "LIV" "Liverpool" "https://resources.premierleague.com/premierleague/badges/t14.svg" 12 6)
        (Team "MCI" "Manchester City" "https://resources.premierleague.com/premierleague/badges/t43.svg" 11 6)
      )
  , Match
      "3"
      (Team "CHE" "Chelsea" "https://resources.premierleague.com/premierleague/badges/t8.svg" 9 7)
      (Team "MUN" "Manchester United" "https://resources.premierleague.com/premierleague/badges/t1.svg" 10 7)
      (Score 4 3)
      "2024-01-22"
      (Statistics
        (Score 54 46)
        (Score 19 11)
        (Score 9 5)
        (Score 9 4)
        (Score 8 12)
        (Score 1 3)
        (Score 0 0)
        (Score 567 489)
        (Score 88 84)
      )
      (MVP "CP20" "Cole Palmer" "2 Goals, 1 Assist, 5 Key Passes, 3 Successful Dribbles" "https://resources.premierleague.com/premierleague/photos/players/250x250/p244851.png")
      (HeadToHead 
        (Team "CHE" "Chelsea" "https://resources.premierleague.com/premierleague/badges/t8.svg" 9 7) 
        (Team "MUN" "Manchester United" "https://resources.premierleague.com/premierleague/badges/t1.svg" 10 7)
      )
  ]

exampleStanding :: [Standing]
exampleStanding =
    [ Standing 1 "Liverpool" 21 48 "https://resources.premierleague.com/premierleague/badges/t14.svg" ["W", "W", "D", "W", "W"] 25
    , Standing 2 "Manchester City" 20 46 "https://resources.premierleague.com/premierleague/badges/t43.svg" ["W", "D", "W", "W", "D"] 24
    , Standing 3 "Arsenal" 21 43 "https://resources.premierleague.com/premierleague/badges/t3.svg" ["W", "L", "W", "W", "W"] 17
    , Standing 4 "Aston Villa" 21 43 "https://resources.premierleague.com/premierleague/badges/t7.svg" ["L", "W", "W", "L", "W"] 16
    , Standing 5 "Tottenham" 21 40 "https://resources.premierleague.com/premierleague/badges/t6.svg" ["L", "W", "L", "W", "W"] 12
    ]

exampleUpcomingMatches :: [UpcomingMatch]
exampleUpcomingMatches =
    [ UpcomingMatch 1 "Arsenal" "Liverpool" "2024-02-04" "16:30"
        "Premier League" "Emirates Stadium"
        "https://resources.premierleague.com/premierleague/badges/t3.svg"
        "https://resources.premierleague.com/premierleague/badges/t14.svg"
        "Sold Out" "Sky Sports Main Event"
    , UpcomingMatch 2 "Manchester City" "Brentford" "2024-02-05" "20:00"
        "Premier League" "Etihad Stadium"
        "https://resources.premierleague.com/premierleague/badges/t43.svg"
        "https://resources.premierleague.com/premierleague/badges/t94.svg"
        "Available" "BT Sport 1"
    , UpcomingMatch 3 "Tottenham" "Brighton" "2024-02-10" "15:00"
        "Premier League" "Tottenham Hotspur Stadium"
        "https://resources.premierleague.com/premierleague/badges/t6.svg"
        "https://resources.premierleague.com/premierleague/badges/t36.svg"
        "Limited Availability" "Not Televised"
    ]

exampleMatchAnalysis :: [MatchAnalysis]
exampleMatchAnalysis =
    [ MatchAnalysis 
        "1"
        "North London Derby Tactical Analysis"
        "A thrilling encounter that showcased Arsenal's pressing system against Tottenham's counter-attacking approach"
        [ "Arsenal's high press led by Saka and Martinelli"
        , "Tottenham's effective transitions through Son"
        , "Rice's crucial role in controlling midfield"
        , "Set-piece dominance from Arsenal"
        ]
        (ClubCompetition "4-3-3" "4-2-3-1")
        [ KeyBattle 
            "Midfield Control"
            (ClubCompetition "Declan Rice" "James Maddison")
            "Rice's positioning nullified Maddison's creative influence"
        ]
        (HeatMap (ClubCompetition "Left Wing Focus" "Counter-attacks through Right"))
    ]

findMatch :: Text -> [Match] -> Maybe Match
findMatch path_matchId matches = find (\eachMatch -> matchId_match eachMatch == path_matchId) matches    

-- Definición de los endpoints
app :: ScottyM ()
app = do
    get "/matches" $ do
        json exampleMatches
    
    get "/match/:matchId" $ do
        path_matchId <- pathParam "matchId"
        case (findMatch) (path_matchId) (exampleMatches) of
            Just eachMatch  -> json eachMatch
            Nothing         -> status notFound404

    get "/standings" $ do
        json exampleStanding

    get "/upcoming" $ do
        json exampleUpcomingMatches
    
    get "/analysis" $ do
        json exampleMatchAnalysis
    
    get "/favicon.ico" $ do
        setHeader "Content-Type" "image/x-icon"
        raw ""
    
    {-
    get "/fixtures" $ do
        fixtures <- liftIO getFixtures
        case fixtures of
            Just fs -> json fs
            Nothing -> status notFound404
    
    -- Endpoint para obtener los resultados de los partidos
    get "/match-results" $ do
        matches <- liftIO getMatches
        case matches of
            Just ms -> json ms
            Nothing -> status notFound404
    
    -- Endpoint para obtener los clubes
    get "/badges" $ do
        clubs <- liftIO getClubs
        case clubs of
            Just cs -> json cs
            Nothing -> status notFound404
    
    -- Endpoint para obtener estadísticas de un partido específico
    get "/match-stats/:matchId" $ do
        matchId <- pathParam "matchId" 
        stats <- liftIO $ getMatchStatistics matchId
        
        case stats of
            Just st -> json st 
            Nothing -> status notFound404 
    
    -- Endpoint para obtener la clasificación de la liga
    get "/standings" $ do
        standings <- liftIO getStandings
        case standings of
            Just st -> json st
            Nothing -> status notFound404
    
    -- Endpoint para obtener el MVP por pollId
    get "/poll-mvp/:pollId" $ do
        path_pollId <- pathParam "pollId"
        mvp <- liftIO $ getMVP_ID path_pollId
        case mvp of
            Just mv -> json mv
            Nothing -> status notFound404

    -- Endpoint para obtener los enfrentamientos históricos
    get "/scrape" $ do
        matchIdsParam <- queryParam "matchIds"  
        
        headsToHeads <- liftIO $ getScrape matchIdsParam
        
        case headsToHeads of
            Just ht -> json ht  
            Nothing -> status notFound404 
    
    -- Endpoint para obtener estadísticas MVP de un jugador en un partido específico
    get "/mvp-stats/:playerId/:matchId" $ do
        path_playerId <- pathParam "playerId"
        path_matchId <- pathParam "matchId"
        mvpStats <- liftIO $ getMVP path_playerId path_matchId
        case mvpStats of
            Just ms -> json ms
            Nothing -> status notFound404
    -}