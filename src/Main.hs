{-# LANGUAGE OverloadedStrings #-}

import BoX11
import Foreign.C.Types
import Data.Word

main = do
    putStrLn "getWins:"
    wins <- getWins ".*" 3 
    print wins

    putStrLn "getWinsBy:"
    getWinsBy (\h -> return $ (h == 65586)) >>= print

    putStrLn "getCursorPos:"
    getCursorPos >>= print

    -- putStrLn "messageBox:"
    -- messageBox "ñmsg" "ñtitle" >>= print

    putStrLn "sendKey:"
    sendKey (head wins) (241 :: Word8) (241 :: Word8)

    putStrLn "getName:"
    sequence (getName <$> wins) >>= print

    putStrLn "getClass:"
    sequence (getClass <$> wins) >>= print

    return ()
