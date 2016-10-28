{-# LANGUAGE OverloadedStrings #-}
import BoX11.X
import XHotkey
import Data.NMap
import Data.Foldable

main = runX' $ do
    wins <- getWins byClassEx "GxWindowClassD3d\\|GxWindowClassOpenGl"
    io $ print wins
    setBindings $ fromList
            [ read "q" .> exitX
            , read "w" .> (io $ print 1) >> traverse_ sendKeyDown wins
            , up_ (read "w") .> (io $ print 2) >> traverse_ sendKeyUp wins
            , read "scroll-W" .> io $ print "DOWN"
            , (scroll_ $ up_ $ read "W") .> io $ print "up"
            , read ("scroll-W") .> io $ print "down" ]
    mainLoop
    return ()
