{-# LANGUAGE OverloadedStrings #-}

module APIProcessing.StandingProcess
    ( convertToStanding
    , getStandingsWithClubs
    ) where

import FootballAPI ( getClubs, getStandings ) 
import FetchData.ClubAPI ( Club(..) )
import FetchData.PremierLeagueTableAPI
import Data.JSONOtherType ( Standing(..) ) 
import Data.Maybe ( mapMaybe )
import qualified Data.HashMap.Strict as HM

convertToStanding :: PremierLeagueTeam -> Club -> Standing
convertToStanding (PremierLeagueTeam (TeamName name) position (Overall played points goalDiff) formList) (Club _ shield) =
    Standing position name played points shield (map outcome_form formList) goalDiff

getStandingsWithClubs :: IO [Standing]
getStandingsWithClubs = do
    maybeStandings <- getStandings             
    maybeClubs <- getClubs                  

    let standings = maybe [] id maybeStandings 
        clubsList = maybe [] HM.elems maybeClubs
        clubsMap = HM.fromList [(cname, club) | club@(Club cname _) <- clubsList] 
        findClub team = HM.lookup (getTeamName team) clubsMap
        getTeamName (PremierLeagueTeam (TeamName name) _ _ _) = name

    return $ mapMaybe (\team -> fmap (convertToStanding team) (findClub team)) standings
