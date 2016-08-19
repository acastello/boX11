{-# LANGUAGE OverloadedStrings #-}

import BoX11

main = do
    getWins ".*" 3 >>= print
