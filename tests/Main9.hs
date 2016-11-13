{-# LANGUAGE OverloadedStrings #-}
import XHotkey
import BoX11.X
import Data.NMap
import Data.Foldable

keys :: [KM]
keys = (read <$> ["Return", "BackSpace", "Tab", "space", "ctrl-mouse1"]) ++ (enumFromTo (read "a") (read "z")) ++ (enumFromTo (read "0") (read "9"))

movement :: [KM]
movement = alt_ <$> read <$> ["w", "a", "s", "d"]

exit :: Bindings
exit = branch [read "ctrl-alt-scroll-BackSpace"] exitX

b1 ws = fromList [ (scroll_ k) .> broadcast ws | k <- keys ]
bmov ws = mconcat $ 
        [ \km -> branch [scroll_ $ up_ km] $ broadcastS ws
        , \km -> branch [scroll_ km]       $ broadcastS ws ] 
            <*> movement

main = runX' $ do
    wins <- getWins byClassEx "GxWindowClass.*"
    io $ print wins
    
    -- binds' <- binds wins
    -- setBindings $ insert0 (read "q") exitX binds'
    setBindings $ mconcat [exit, bmov wins, b1 wins]
    printBindings
    mainLoop
    
