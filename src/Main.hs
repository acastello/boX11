{-# LANGUAGE OverloadedStrings #-}

import BoX11
import Foreign.C.Types
import Data.Word
import Control.Concurrent

main = do
    putStrLn "getWins:"
    wins <- getWins ".*" 3 
    print wins

    putStrLn "getWinsBy:"
    print =<< (getWinsBy $ \h -> do
        name <- getClass h
        return (name == "GxWindowClass"))

    putStrLn "getCursorPos:"
    getCursorPos >>= print

    -- putStrLn "messageBox:"
    -- messageBox "ñmsg" "ñtitle" >>= print

    putStrLn "sendKey:"
    sequence ((\w -> sendKey w (95 :: Word8) (95 :: Word8)) <$> wins)

    putStrLn "getName:"
    sequence (getName <$> wins) >>= print

    putStrLn "getClass:"
    sequence (getClass <$> wins) >>= print

    return ()
