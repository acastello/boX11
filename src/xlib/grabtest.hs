import Foreign
import Graphics.X11

main = do
    dpy <- openDisplay ""
    let root = defaultRootWindow dpy
    grabKey dpy 24 0 root False grabModeAsync grabModeAsync
    grabKey dpy 32 anyModifier root False grabModeAsync grabModeAsync
    let loop e = do
        nextEvent dpy e
        kev@(_root, _child, _time, _x, _y, _rx, _ry, _mod, _kc, _sc) <- get_KeyEvent e
        print kev
        print =<< lookupString (castPtr e)
        case _kc of
            24 -> putStrLn "bye."
            otherwise -> do
                sendEvent dpy root False 2 e 
                loop e
        
    allocaXEvent loop

    ungrabKey dpy 24 0 root
    ungrabKey dpy 32 anyModifier root
    closeDisplay dpy
    return ()
