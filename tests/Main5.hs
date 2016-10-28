{-# LANGUAGE OverloadedStrings #-}
import XHotkey
import BoX11.X
import BoX11.Basic (byClassEx)
import Data.NMap

b :: Bindings
b = fromList
    [ read "1" .> clickWins 1 =<< getWins byClassEx "GxWindowClassD3d\\|GxWindowClassOpenGl" 
    , read "m" .> printBindings
    , read "w" .> io $ putStrLn "..."
    , read "w up" .> io . print =<< getWins byClassEx "GxWindowClass.*"
    , read "q" .> exitX
    ]

b2 :: Bindings
b2 = fromList
    [ read "m" .> io $ putStrLn "m..." ]

main = do
    runX' $ setBindings (mconcat [b,b2]) >> mainLoop
    return ()
