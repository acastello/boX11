module XHotkey.Core where

import XHotkey.Types
import MapTree

import Graphics.X11
import Graphics.X11.Xlib.Extras

import Data.Word

import Control.Monad.State
import Control.Monad.Reader

import Control.Concurrent
import Foreign
import qualified Data.List as L
import qualified Data.Map as M

runX :: (X a) -> XEnv -> XControl -> IO (a, XControl)
runX (X a) env control = runStateT (runReaderT a env) control

runX' :: (X a) -> IO a
runX' m = do
    dpy <- openDisplay ""
    let root = defaultRootWindow dpy
    (ret, st) <- allocaXEvent $ \e -> fillBytes e 0 196 >> runX m (XEnv dpy root e 0) (XControl mempty False)
    closeDisplay dpy
    return ret

mainLoop :: X ()
mainLoop = do
    XControl { hkMap = hk } <- get
    loop (baseKeys hk)
    return ()
    where 
      loop :: [KM] -> X ()
      loop hk = do
        XControl { hkMap = hk', exitScheduled = ext } <- get
        if ext then
            return ()
        else do
            mapM_ _grabKM (L.intersect hk (baseKeys hk'))
            mapM_ _ungrabKM (hk L.\\ (baseKeys hk'))
            XEnv { display = dpy, rootWindow' = root, currentEvent = ptr } <- ask
            liftIO $ nextEvent dpy ptr
            evt <- liftIO $ get_EventType ptr
            if evt == buttonPress or evt == keyPress then
                hk'!(read "c/")
            else
                loop hk'
            
    
_grabKM :: KM -> X ()
_grabKM k = do
    XEnv { display = dpy, rootWindow' = root } <- ask
    (KM _ st k') <- normalizeKM k
    case k' of
        KCode c -> liftIO $ grabKey dpy c st root False grabModeAsync grabModeAsync
        MButton b -> liftIO $ grabButton dpy b st root False (buttonPressMask .|. buttonReleaseMask) grabModeAsync grabModeAsync root 0

_ungrabKM :: KM -> X ()
_ungrabKM k = do
    XEnv { display = dpy, rootWindow' = root } <- ask
    (KM _ st k') <- normalizeKM k
    case k' of
        KCode c -> liftIO $ ungrabKey dpy c st root
        MButton b -> liftIO $ ungrabButton dpy b st root
    return ()

flushX :: X ()
flushX = do
    XEnv { display = dpy } <- ask
    liftIO $flush dpy
    

pointerPos :: X (Position, Position)
pointerPos = do
    XEnv { display = dpy, rootWindow' = root, currentEvent = ptr} <- ask
    liftIO $ do
        typ <- if ptr == nullPtr then return 0 else get_EventType ptr
        if typ > 2 then do
            ev <- getEvent ptr
            return (fromIntegral $ ev_x_root ev, fromIntegral $ ev_y_root ev)
        else do
            (_, _, _, x, y, _, _, _) <- queryPointer dpy root
            return (fromIntegral x,fromIntegral y)

relPointerPos :: Window -> X (Position, Position)
relPointerPos w = do
    XEnv { display = dpy } <- ask
    liftIO $ do
        (_,_,_,_,_, x, y, _) <- queryPointer dpy w
        return (fromIntegral x, fromIntegral y)
