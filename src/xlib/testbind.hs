import Graphics.X11
import Data.Map (Map)
import qualified Data.Map as M
import Data.IORef

kevCB :: IORef (Map (Modifier, KeyCode) (IO ())) -> XKeyEvent -> IO ()
kevCB ref (_,_,_,_,_,_,_, mod, kc, _) = do
    m <- readIORef ref
    case M.lookup (mod, kc) m of
        Nothing -> return ()
        Just f -> f

mainLoop :: IORef (Map (Modifier, KeyCode) (IO ())) -> IO () 
mainLoop ref = do
    dpy <- openDisplay ""
    let loop e = do
            nextEvent dpy e
            typ <- get_EventType e
            if (typ == keyPress || typ == keyRelease) then do
                kev <- get_KeyEvent e
                kevCB ref kev
                loop e
            else 
                loop e
    allocaXEvent loop
        
