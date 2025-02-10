module Main where

import Web.Scotty ( scotty )
import Endpoints ( app )

main :: IO ()
main = do
    scotty 8000 app