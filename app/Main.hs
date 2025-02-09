module Main where

import Web.Scotty ( scotty )
import Endpoints ( app )

main :: IO ()
main = scotty 8000 app