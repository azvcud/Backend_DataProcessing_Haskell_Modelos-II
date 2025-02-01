{-# LANGUAGE OverloadedStrings #-}

module Endpoints
    ( app
    ) where

import Web.Scotty
import Data.Text.Lazy (Text)
import Data.Response (Response(..))

-- Definición de los endpoints
app :: ScottyM ()
app = do
    get "/json" $ do
        let jsonResponse = Response "Hello, world!" "success"
        json jsonResponse