import Graphics.X11
import Graphics.X11.Xlib.Extras
import Control.Concurrent

main = do
    dpy <- openDisplay ""
    setErrorHandler (\dpy ptr -> print ptr)
    let root = defaultRootWindow dpy
    grabKey dpy 32 anyModifier root False grabModeAsync grabModeAsync
    flush dpy
    grabKey dpy 32 anyModifier root False grabModeAsync grabModeAsync
    flush dpy
    threadDelay 2500000
    ungrabKey dpy 0 anyModifier root 
    flush dpy
    threadDelay 2500000
    closeDisplay dpy
    
