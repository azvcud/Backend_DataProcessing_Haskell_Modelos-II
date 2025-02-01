{-# LANGUAGE OverloadedStrings #-}

module Endpoints
    ( app
    ) where

import Network.HTTP.Types.Status (notFound404)
import Web.Scotty
import Data.Text.Lazy (Text)
import Data.List (find)
import Data.JSONType 
    ( Match(..)
    , Team(..)
    , Score(..)
    , Statistics(..)
    , MVP(..)
    , Highlights(..)
    , Player(..)
    )

-- JSON de Ejemplo
exampleMatches :: [Match]
exampleMatches =
    [ Match
        { matchId_match = "1"
        , homeTeam_match = Team "ARS" "Arsenal" "https://resources.premierleague.com/premierleague/badges/t3.svg"
        , awayTeam_match = Team "CHE" "Chelsea" "https://resources.premierleague.com/premierleague/badges/t8.svg"
        , score_match = Score 2 1
        , date_match = "2024-01-20"
        , statistics_match = Statistics
            (Score 58 42)
            (Score 15 8)
            (Score 7 3)
            (Score 8 4)
            (Score 10 12)
            (Score 2 3)
            (Score 0 0)
            (Score 523 432)
            (Score 87 82)
        , mvp_match = MVP "BS7" "Bukayo Saka" "1 Goal, 1 Assist, 8 Key Passes"
        , highlights_match = Highlights
            "1"
            (Player "BS7" "Bukayo Saka" "Forward" "https://resources.premierleague.com/premierleague/photos/players/250x250/p223340.png")
            "Brilliant goal from outside the box"
            "23"
        }
    ]

findMatch :: Text -> [Match] -> Maybe Match
findMatch path_matchId matches = find (\eachMatch -> matchId_match eachMatch == path_matchId) matches    

-- Definición de los endpoints
app :: ScottyM ()
app = do
    get "/matches" $ do
        json exampleMatches
    
    get "/matches/:matchId" $ do
        path_matchId <- pathParam "matchId"
        case (findMatch) (path_matchId) (exampleMatches) of
            Just eachMatch  -> json eachMatch
            Nothing         -> status notFound404
    
    get "/matches/:matchId/statistics" $ do
        path_matchId <- pathParam "matchId"
        case (findMatch) (path_matchId) (exampleMatches) of
            Just eachMatch  -> json (statistics_match eachMatch)
            Nothing         -> status notFound404
    
    get "/matches/:matchId/highlights" $ do
        path_matchId <- pathParam "matchId"
        case (findMatch) (path_matchId) (exampleMatches) of
            Just eachMatch  -> json (highlights_match eachMatch)
            Nothing         -> status notFound404

    get "/favicon.ico" $ do
        setHeader "Content-Type" "image/x-icon"
        raw ""