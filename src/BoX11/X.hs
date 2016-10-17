module BoX11.X where

import XHotkey
import qualified BoX11.Basic as B
import BoX11.Basic (Flags, byName, byClass, byNameEx, byClassEx, HWND)

import Data.Word
import Data.ByteString
import Control.Monad.IO.Class
import Control.Concurrent

getWins :: Flags -> ByteString -> X [HWND]
getWins flags str = liftIO $ B.getWins flags str 

getWinsBy :: (HWND -> IO Bool) -> X [HWND]
getWinsBy = liftIO . B.getWinsBy

getCursorPos :: X (Word, Word)
getCursorPos = liftIO B.getCursorPos

clickWins :: Word32 -> [HWND] -> X ()
clickWins k wins = inCurrentPos $ do
    (xp, yp) <- pointerProp
    traverse (liftIO . B.clickProp k xp yp) wins
    io $ threadDelay 80000
    return ()
    

