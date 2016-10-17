{-# LANGUAGE OverloadedStrings #-}
import XHotkey
import BoX11.X
import BoX11.Basic (byClassEx, moveMouse, sendClick)
import Data.NMap

b :: Bindings
b = fromList
    [ read "1" .> inCurrentPos $ do
        wins <- getWins byClassEx "GxWindowClassD3d\\|GxWindowClassOpenGl" 
        (xp,yp) <- pointerProp
        traverse (io . moveMouse xp yp) wins
        return ()
    , read "2" .> do
        wins <- getWins byClassEx "GxWindowClassD3d\\|GxWindowClassOpenGl" 
        (xp,yp) <- pointerProp
        traverse (io . sendClick 1) wins
        return ()
    , read "q" .> exitX
    ]

main = do
    runX' $ setBindings b >> mainLoop
    return ()
