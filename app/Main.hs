module Main where

import Web.Scotty
import Endpoints (app)

main :: IO ()
main = scotty 8000 app