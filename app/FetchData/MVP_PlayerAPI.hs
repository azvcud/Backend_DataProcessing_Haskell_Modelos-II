{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module FetchData.MVP_PlayerAPI 
    ( PlayerInfo(..)
    , PlayerName(..)
    , PlayerEntity(..)
    , MVPStats(..)
    ) where
      
import Data.Aeson ( FromJSON, ToJSON )

import GHC.Generics (Generic)

-- Modelo para la información del jugador (camiseta, posición, imagen)
data PlayerInfo = PlayerInfo
  { shirtNum :: Int
  , positionInfo :: String
  , imageUrl :: String
  } deriving (Show, Generic)

instance FromJSON PlayerInfo
instance ToJSON PlayerInfo

-- Modelo para el nombre del jugador
data PlayerName = PlayerName
  { display :: String
  } deriving (Show, Generic)

instance FromJSON PlayerName
instance ToJSON PlayerName

-- Modelo para la entidad completa (jugador)
data PlayerEntity = PlayerEntity
  { info :: PlayerInfo
  , name :: PlayerName
  } deriving (Show, Generic)

-- El modelo para la respuesta completa
data MVPStats = MVPStats
  { entity :: PlayerEntity
  } deriving (Show, Generic)

instance FromJSON PlayerEntity
instance ToJSON PlayerEntity

instance FromJSON MVPStats
instance ToJSON MVPStats