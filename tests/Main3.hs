{-# LANGUAGE OverloadedStrings #-}
import BoX11.Basic
import Control.Monad

ch = 'α'

main = do
    wins <- getWins byClassEx "GxWindowClass3d\\|GxWindowClassOpenGl"
    print wins
    let loop = do
        str <- getLine
        (sendKeyChar 186 ch) `traverse` wins
        when (str /= "q") loop
        return ()
    loop
