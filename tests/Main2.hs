{-# LANGUAGE OverloadedStrings #-}
import BoX11
import Control.Monad

main = do
    wins <- getWins byClassEx "GxWindowClassD3d\\|GxWindowClassOpenGl\\|Notepad"
    print wins
    let loop = do
        str <- getLine
        traverse (sendKey (fromIntegral $ fromEnum 'Q')) wins
        traverse (sendChar (fromIntegral $ fromEnum 'Q')) wins
        when (str /= "q") loop
        return ()
    loop
        
