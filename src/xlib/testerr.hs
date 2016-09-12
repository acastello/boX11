import Graphics.X11
import Control.Concurrent

main = do
    dpy <- openDisplay ""
    let root = defaultRootWindow dpy
    grabKey dpy 32 anyModifier root False grabModeAsync grabModeAsync
    flush dpy
    grabKey dpy 32 anyModifier root False grabModeAsync grabModeAsync
    flush dpy
    threadDelay 2500000
    closeDisplay dpy
    
