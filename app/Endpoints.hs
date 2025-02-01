{-# LANGUAGE OverloadedStrings #-}

module Endpoints
    ( app
    ) where

import Web.Scotty
import Data.JSONType (Response(..), UserInfo(..))

-- Definición de los endpoints
app :: ScottyM ()
app = do
    get "/json" $ do
        let jsonResponse = Response "Hello, world!" "success"
        json jsonResponse

    get "/user" $ do
        let userInfo = UserInfo "Alice" 42
        json userInfo