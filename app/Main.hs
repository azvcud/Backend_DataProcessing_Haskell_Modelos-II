{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import Web.Scotty
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Data.Text.Lazy (Text)

-- Definición del tipo de dato
data Response = Response
    { message :: Text
    , status  :: Text
    } deriving (Generic)

instance ToJSON Response

main :: IO ()
main = scotty 3000 $ do
    get "/json" $ do
        let jsonResponse = Response "Hello, world!" "success"
        json jsonResponse