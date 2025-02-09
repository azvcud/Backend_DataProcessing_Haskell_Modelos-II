{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module FetchData.ClubAPI
    ( Club(..)
    ) where

import Data.Aeson
    ( (.:),
      withObject,
      object,
      FromJSON(parseJSON),
      KeyValue((.=)),
      ToJSON(toJSON)
    )

import GHC.Generics (Generic)
import Data.Text.Lazy (Text)

data Club = Club
  { name_club :: Text
  , logo_club :: Text
  } deriving (Show, Generic)

instance FromJSON Club where
    parseJSON = withObject "Club" $ \v -> Club
        <$> v .: "name"
        <*> v .: "logo"

instance ToJSON Club where
    toJSON (Club name logo) =
        object [ "name" .= name
               , "logo" .= logo
               ]