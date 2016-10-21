{-# LANGUAGE OverloadedStrings #-}
import XHotkey
import BoX11.X
import Data.NMap
import Data.Foldable

keys :: [KM]
keys = [read "Return", read "BackSpace", read "Tab", read "space"] ++ (enumFromTo (read "a") (read "z")) ++ (enumFromTo (read "0") (read "9"))

movement :: [KM]
movement = read <$> ["W", "A", "S", "D"]

exit :: Bindings
exit = branch [read "ctrl-alt-scroll-BackSpace"] exitX

b1 ws = fromList [ (scroll_ k) .> traverse_ sendKeyChar ws | k <- keys ]
bmov ws = mconcat $ [\km -> branch [scroll_ $ up_ km] $ traverse_ sendKeyUp ws, \km -> branch [scroll_ km] $ traverse_ sendKeyDown ws] <*> movement

main = runX' $ do
    wins <- getWins byClassEx "GxWindowClass.*"
    io $ print wins
    
    -- binds' <- binds wins
    -- setBindings $ insert0 (read "q") exitX binds'
    setBindings $ mconcat [exit, bmov wins, b1 wins]
    printBindings
    mainLoop
    
