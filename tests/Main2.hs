{-# LANGUAGE OverloadedStrings #-}
import BoX11
import Control.Monad

main = do
    wins <- getWins "GxWindowClassD3d\\|Notepad" 3
    print wins
    let loop = do
        str <- getLine
        traverse (\w -> sendKey w (fromIntegral $ fromEnum 'Q') (fromIntegral $ fromEnum 'Q')) wins
        when (str /= "q") loop
        return ()
    loop
        
