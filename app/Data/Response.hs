{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Data.Response
    ( Response(..)
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