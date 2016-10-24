{-# LANGUAGE OverloadedStrings #-}
import XHotkey
import BoX11.X
import BoX11.Basic (byClassEx)
import Data.NMap

b :: Bindings
b = fromList
    [ read "1" .> clickWins 1 =<< getWins byClassEx "GxWindowClassD3d\\|GxWindowClassOpenGl" 
    , read "m" .> printBindings
    , read "w" .> io . print =<< getWins byClassEx "GxWindowClass.*"
    , read "q" .> exitX
    ]

main = do
    runX' $ setBindings b >> mainLoop
    return ()
