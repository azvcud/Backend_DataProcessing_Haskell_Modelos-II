{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.JSONType
    ( Response(..)
    , UserInfo(..)
    ) where

import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Data.Text.Lazy (Text)

-- Definición del tipo de dato
data Response = Response
    { message :: Text
    , status  :: Text
    } deriving (Generic)

instance ToJSON Response

-- Nuevo tipo de dato
data UserInfo = UserInfo
    { nombre :: Text
    , numero :: Int
    } deriving (Generic)

instance ToJSON UserInfo