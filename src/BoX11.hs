{-# LANGUAGE OverloadedStrings #-}

module BoX11 (
    getWins
    ) where

import Foreign
import Foreign.C
import Foreign.C.String
import Foreign.Marshal.Array

import Data.ByteString as BS

foreign import ccall "getWins" _getWins :: CString -> CInt -> IO (Ptr Word64)


getWins :: ByteString -> CInt -> IO [Word64]
getWins bs flags = do
    ptr <- useAsCString bs (\p -> _getWins p flags)
    peekArray0 0 ptr
