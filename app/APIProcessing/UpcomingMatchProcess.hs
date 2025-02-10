module APIProcessing.UpcomingMatchProcess
    ( createUpcomingMatches )
    where

import FootballAPI ( getFixtures, getClubs )       
import FetchData.ScheduleAPI (Fixture(..)) 
import FetchData.ClubAPI ( Club(..) )
import Data.JSONOtherType (UpcomingMatch(..)) 
import Data.Text.Lazy (Text, splitOn)
import qualified Data.HashMap.Strict as HashMap
import Control.Monad (forM)

-- Función para separar la fecha y la hora
splitDateTime :: Text -> (Text, Text)
splitDateTime dateTime =
    case splitOn "," dateTime of
        [d, t] -> (d, t)  -- Si se separa en dos partes (fecha y hora)
        _      -> (dateTime, "TBD") -- Si el formato es inválido

-- Convertir Fixture a UpcomingMatch
convertToUpcomingMatch :: Int -> Fixture -> Club -> Club -> Text -> Text -> Text -> Text -> UpcomingMatch
convertToUpcomingMatch matchId (Fixture home away matchTime) (Club _ homeShield) (Club _ awayShield) competition venue ticketStatus =
    let (date, time) = splitDateTime matchTime
    in UpcomingMatch matchId home away date time competition venue homeShield awayShield ticketStatus

-- Crear UpcomingMatches
createUpcomingMatches :: IO [UpcomingMatch]
createUpcomingMatches = do
    maybeFixtures <- getFixtures
    maybeClubs <- getClubs
    case (maybeFixtures, maybeClubs) of
        (Just fixtures, Just clubs) -> do
            let clubMap = HashMap.fromList [(name_club club, club) | (_, club) <- HashMap.toList clubs]
            
            forM (zip [1..] fixtures) $ \(matchId, fixture) -> do
                let homeClub = HashMap.lookupDefault (Club (homeTeam_fixture fixture) "") (homeTeam_fixture fixture) clubMap
                let awayClub = HashMap.lookupDefault (Club (awayTeam_fixture fixture) "") (awayTeam_fixture fixture) clubMap
                return $ convertToUpcomingMatch matchId fixture homeClub awayClub "Premier League" "Unknown Venue" "Available" "BT Sport"
        _ -> return []
