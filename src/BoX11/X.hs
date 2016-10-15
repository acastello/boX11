module BoX11.X where

import XHotkey
import qualified BoX11.Basic as B
import BoX11.Basic (Flags, byName, byClass, byNameEx, byClassEx, HWND)

import Data.ByteString
import Control.Monad.IO.Class

getWins :: Flags -> ByteString -> X [HWND]
getWins flags str = liftIO $ B.getWins flags str 

getWinsBy :: (HWND -> IO Bool) -> X [HWND]
getWinsBy = liftIO . B.getWinsBy

getCursorPos :: X (Word, Word)
getCursorPos = liftIO B.getCursorPos

cursorProp :: X (Double, Double)
cursorProp

