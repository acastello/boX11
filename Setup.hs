import Distribution.Simple
import Distribution.Simple.Setup
import Distribution.Simple.LocalBuildInfo
import Distribution.Simple.Program.Run
import Distribution.Verbosity

import Control.Monad (when)

main = defaultMainWithHooks simpleUserHooks 
    { postBuild = \args flags desc info -> do
        let LocalBuildInfo { buildDir = dist } = info
            BuildFlags { buildVerbosity = Flag v } = flags
            cmd = concat ["winegcc -shared -fPIC src/boX11.c -o " 
                         , dist
                         , "/libboX11.dll.so /usr/lib/wine/user32.dll.so" 
                         , (if v >= verbose then " -v" else "") 
                         , "; mv " 
                         , dist
                         , "/libboX11.dll.so " 
                         , dist 
                         , "/libboX11.so" ]
            cmd2 = "winegcc -ldl -Wl,-rpath=. src/boxlaunch.c -o " ++ dist ++ "/boxlaunch.exe"
        when (v /= silent) $ putStrLn cmd
        runProgramInvocation silent $ simpleProgramInvocation "/bin/sh" ["-c", cmd]
        runProgramInvocation silent $ simpleProgramInvocation "/bin/sh" ["-c", cmd2]

    , postCopy = \args flags desc info -> do
        let LocalBuildInfo 
                { buildDir = dist
                , installDirTemplates = InstallDirs { libdir = d, bindir = b, libsubdir = sd }} = info
            CopyFlags { copyVerbosity = Flag v } = flags
            dir = fromPathTemplate d
            subdir = fromPathTemplate sd
            bindir = fromPathTemplate b
            targetdir = dir ++ "/" ++ subdir
            cmd = "cp " ++ dist ++ "/libboX11.so " ++ targetdir
            cmd2 = "cp " ++ dist ++ "/boxlaunch.exe " ++ dist ++ "/boxlaunch.exe.so " ++ bindir

        when (v /= silent) $ putStrLn cmd >> putStrLn cmd2
        runProgramInvocation silent $ simpleProgramInvocation "/bin/sh" ["-c", cmd]        
        runProgramInvocation silent $ simpleProgramInvocation "/bin/sh" ["-c", cmd2]        
    }

