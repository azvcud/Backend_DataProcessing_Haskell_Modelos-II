module Main where

import Web.Scotty ( scotty )
import Endpoints ( app )
import APIProcessing ( printFixtures )

main :: IO ()
main = do
    printFixtures
    scotty 8000 app