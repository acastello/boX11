module BoX11.X
    -- reexports
    ( Flags, byName, byClass, byNameEx, byClassEx, HWND
    ,getWins, getWinsBy, getCursorPos, clickWins, portKM)
    where

import XHotkey
import qualified BoX11.Basic as B
import BoX11.Basic (Flags, byName, byClass, byNameEx, byClassEx, HWND, VK)
import Graphics.X11

import Data.Word
import Data.ByteString
import Data.Foldable
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Monad.State
import Control.Monad.Reader

getWins :: Flags -> ByteString -> X [HWND]
getWins flags str = liftIO $ B.getWins flags str 

getWinsBy :: (HWND -> IO Bool) -> X [HWND]
getWinsBy = liftIO . B.getWinsBy

getCursorPos :: X (Word, Word)
getCursorPos = liftIO B.getCursorPos

broadcast :: Traversable t => KM -> t HWND -> X ()
broadcast k ws = do
    vk <- portKM k
    if vk <= 6 then
        clickWins (fromIntegral $ if vk > 4 then vk -1 else vk) ws
    else
        pressWins vk ws
    return ()
    
pressWins :: Traversable t => VK -> t HWND -> X ()
pressWins k ws = traverse_ (liftIO . B.sendKey k) ws

clickWins :: Traversable t => Word32 -> t HWND -> X ()
clickWins k wins = inCurrentPos $ do
    (xp, yp) <- pointerProp
    traverse (liftIO . B.clickProp k xp yp) wins
    -- io $ threadDelay 80000
    return ()
    
portKM :: KM -> X VK
portKM (KM u st (KSym ks)) = do
    XEnv { display = dpy } <- ask
    ks' <- io $ flip (keycodeToKeysym dpy) 0 =<< keysymToKeycode dpy ks
    return (fromIntegral ks')
portKM (KM u st (KCode kc)) = do
    XEnv { display = dpy } <- ask
    ks' <- io $ keycodeToKeysym dpy kc 0
    return (fromIntegral ks')
portKM (KM u st (MButton b)) = return (fromIntegral b)

-- sendKey :: Foldable t => KM -> t HWND -> X ()
-- sendKey k ws = do
    -- <F5>


