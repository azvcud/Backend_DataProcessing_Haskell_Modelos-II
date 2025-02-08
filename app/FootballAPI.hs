{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module FootballAPI ( getFixtures ) where

import FetchData.Fixture 
    ( Fixture(..)
    )

import Network.Wreq ( get, responseBody )
import Data.Aeson (decode)
import Control.Lens ( (^.) )

getFixtures :: IO (Maybe [Fixture])
getFixtures = do
    response <- get "http://localhost:3000/fixtures"
    return $ decode (response ^. responseBody)