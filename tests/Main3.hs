{-# LANGUAGE OverloadedStrings #-}
import BoX11.Basic
import Control.Monad

main = do
    wins <- getWins byClassEx "GxWindowClass3d\\|GxWindowClassOpenGl"
    print wins
    let loop = do
        str <- getLine
        traverse (sendKeyDown 0x11) wins
        traverse (sendKey (fromIntegral $ fromEnum 'R')) wins
        traverse (sendKeyUp 0x11) wins
        when (str /= "q") loop
        return ()
    loop
