{-# LANGUAGE OverloadedStrings #-}
import qualified BoX11.Basic as B
import BoX11.X
import XHotkey
import Data.NMap
import Data.Foldable

main = do
    wins <- B.getWins byClassEx "GxWindow.*"
    print wins
    let binds = fromList
            [ read "q" .> exitX
            , read "1" .> io $ print 1
            , read "2" .> io $ (flip traverse_ wins) $ \w -> do
                B.sendKeyDown 0x10 w
                B.sendKey (fromIntegral $ fromEnum '1') w
                B.sendKeyUp 0x10 w
            , read "3" .> io $ (flip traverse_ wins) $ \w -> do
                B.sendKey 0x01 w    
            ]
    runX' $ setBindings binds >> mainLoop
