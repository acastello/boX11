{-# LANGUAGE OverloadedStrings #-}

import BoX11

main = do
    putStrLn "getWins:"
    wins <- getWins ".*" 3
    print wins

    putStrLn "getName:"
    print =<< traverse getName wins

    putStrLn "getClass:"
    print =<< traverse getClass wins
