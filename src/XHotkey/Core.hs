module XHotkey.Core where

import XHotkey.Types
import Graphics.X11
import Graphics.X11.Xlib.Extras

import Control.Monad.State
import Control.Monad.Reader
import System.IO.Error
import qualified Data.Map as M

runX :: (X a) -> XEnv -> XControl -> IO (a, XControl)
runX (X a) env control = runStateT (runReaderT a env) control

runX' :: (X a) -> IO a
runX' m = do
    dpy <- openDisplay ""
    let root = defaultRootWindow dpy
    (ret, st) <- allocaXEvent $ \e -> runX m (XEnv dpy root e) (XControl M.empty False)
    closeDisplay dpy
    return ret

getRootPos :: X (Position, Position)
getRootPos = do
    XEnv { display = dpy, currentEvent = ptr } <- ask
    coords <- liftIO $ catchIOError (do
            ev <- getEvent ptr
            -- return (fromIntegral $ ev_x_root ev, fromIntegral $ ev_y_root ev))
            return (0,0) )
        (\e -> do
            return (0,0))
    return coords
