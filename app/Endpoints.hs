{-# LANGUAGE OverloadedStrings #-}

module Endpoints
    ( app
    ) where

import Web.Scotty
import Data.JSONType 
    ( Match(..)
    , Team(..)
    , Score(..)
    , Standing(..)
    )

import Network.HTTP.Types.Status (notFound404)

-- JSON de Ejemplo
exampleMatches :: [Match]
exampleMatches =
  [ Match
      { matchId = 1
      , homeTeam = Team
          { name = "Liverpool"
          , shield = "https://resources.premierleague.com/premierleague/badges/t14.svg"
          }
      , awayTeam = Team
          { name = "Man City"
          , shield = "https://resources.premierleague.com/premierleague/badges/t43.svg"
          }
      , score = Score
          { home = 2
          , away = 1
          }
      , date = "2023-10-01"
      }
  , Match
      { matchId = 2
      , homeTeam = Team
          { name = "Arsenal"
          , shield = "https://resources.premierleague.com/premierleague/badges/t3.svg"
          }
      , awayTeam = Team
          { name = "Tottenham"
          , shield = "https://resources.premierleague.com/premierleague/badges/t6.svg"
          }
      , score = Score
          { home = 3
          , away = 2
          }
      , date = "2023-10-02"
      }
  ]

exampleStandings :: [Standing]
exampleStandings = 
    [ Standing
        { position = 1
        , team = "Liverpool"
        , played = 21
        , points = 48
        , shield_S = "https://resources.premierleague.com/premierleague/badges/t14.svg"
        }
    , Standing
        { position = 2
        , team = "Man City"
        , played = 20
        , points = 46
        , shield_S = "https://resources.premierleague.com/premierleague/badges/t43.svg"
        }
    , Standing
        { position = 3
        , team = "Arsenal"
        , played = 21
        , points = 43
        , shield_S = "https://resources.premierleague.com/premierleague/badges/t3.svg"
        }
    ]

findMatchById :: Int -> Maybe Match
findMatchById searchId =
    case filter (\m -> searchId == matchId m) exampleMatches of
        [match] -> Just match
        _       -> Nothing

-- Definición de los endpoints
app :: ScottyM ()
app = do
    get "/matches" $ do
        json exampleMatches
    
    get "/matches/:matchId" $ do
        matchIdStr <- pathParam "matchId" :: ActionM String
        let searchId = read matchIdStr :: Int
        case findMatchById searchId of
            Just match -> json match
            Nothing    -> do
                status notFound404
                json ("Match not found" :: String)

    get "/standings" $ do
        json exampleStandings