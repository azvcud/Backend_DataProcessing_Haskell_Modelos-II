module APIProcessing 
    ( printFixtures
    ) where

import FootballAPI ( getFixtures )

printFixtures :: IO ()
printFixtures = do
    result <- getFixtures
    case result of
        Just fixtures -> print fixtures  -- Imprime los fixtures
        Nothing       -> putStrLn "No se encontraron fixtures"  -- Mensaje de error