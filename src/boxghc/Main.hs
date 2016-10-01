import System.Process
import System.Environment

main = do
    let compiler = "ghc"
    args@(target:_) <- getArgs
    vers <- return . (filter (/= '\n')) =<< readProcess compiler ["--numeric-version"] "" 
    let rts = "/usr/lib/ghc-" ++ vers ++ "/rts/libHSrts-ghc" ++ vers ++ ".so"
    print [show args , vers, rts]
    print =<< readProcess "ls" [rts] ""
    return ()
    
