{-# LANGUAGE OverloadedStrings #-}
import BoX11

main = do
    wins <- getWins byClassEx "GxWindowClassD3d\\|GxWindowClassOpenGl"
    print wins
    traverse (sendClick 4) wins
    return ()
