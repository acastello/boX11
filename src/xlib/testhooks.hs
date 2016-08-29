import Graphics.X11

mainLoop = do
    dpy <- openDisplay ""
    let root = defaultRootWindow dpy
    let loop e = do
        nextEvent dpy e
        typ <- get_EventType e
        if (typ == keyPress || typ == keyRelease) then do
            kev <- get_KeyEvent e
            loop e
        else
            loop e
    allocaXEvent loop

    closeDisplay dpy

