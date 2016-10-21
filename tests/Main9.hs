{-# LANGUAGE OverloadedStrings #-}
import XHotkey
import BoX11.X
import Data.NMap

keys :: [KM]
keys = [read "Return"] ++ (enumFromTo (read "a") (read "z")) ++ (enumFromTo (read "0") (read "9"))

keysp 

binds :: Traversable t => t HWND -> X Bindings
binds ws = sequence $ fromList [a .> ($ ws) <$> (broadcast a) | a <- keys]
main = runX' $ do
    wins <- getWins byClassEx "GxWindowClass.*"
    io $ print wins
    binds' <- binds wins
    setBindings $ insert0 (read "q") exitX binds'
    printBindings
    mainLoop
    
