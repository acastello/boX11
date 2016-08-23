{-# LANGUAGE OverloadedStrings #-}

module BoX11 (
    getWins,
    getWinsBy,
    getCursorPos,
    messageBox,
    sendKey,
    getName,
    getClass,
    ) where

import Foreign
import Foreign.C
import Foreign.C.String
import Foreign.Marshal.Array

import Data.Bits
import Data.ByteString as BS

type HWND = Word64

--------------------------------------------------------------------------------
-- getWins  
--------------------------------------------------------------------------------

getWins :: ByteString -> CInt -> IO [Word64]
getWins bs flags = do
    ptr <- useAsCString bs (\p -> getWins' p flags)
    peekArray0 0 ptr

foreign import ccall unsafe "getWins" getWins' :: CString -> CInt -> IO (Ptr Word64)

--------------------------------------------------------------------------------
-- getWinsBy  
--------------------------------------------------------------------------------

getWinsBy :: (Word64 -> IO Bool) -> IO [Word64]
getWinsBy f = do
    let f' h = do
        ret <- f h
        return $ case ret of
            True -> (1 :: CInt)
            False -> (0 :: CInt)
    f'' <- mkF f'
    ptr <- getWinsBy' f''
    peekArray0 0 ptr
    
foreign import ccall "wrapper" mkF :: (Word64 -> IO CInt) -> IO (FunPtr (Word64 -> IO CInt))
foreign import ccall "getWinsBy" 
    getWinsBy' :: FunPtr (Word64 -> IO CInt) -> IO (Ptr Word64)

--------------------------------------------------------------------------------
-- getCursorPos  
--------------------------------------------------------------------------------

getCursorPos :: IO (Word, Word)
getCursorPos = do
    pos <- getCursorPos'
    return (fromIntegral $ pos .&. 0xff, fromIntegral $ shiftR pos 32)

foreign import ccall unsafe "getCursorPos"
    getCursorPos' :: IO Word64

--------------------------------------------------------------------------------
-- messageBox  
--------------------------------------------------------------------------------

messageBox :: ByteString -> ByteString -> IO CInt
messageBox msg title = do
    useAsCString msg $ \msg' -> do
        useAsCString title $ \title' ->
            messageBox' (msg') (title') 0
            
foreign import ccall safe "messageBox" 
    messageBox' :: CString -> CString -> CInt -> IO CInt

--------------------------------------------------------------------------------
-- sendKey
--------------------------------------------------------------------------------

foreign import ccall unsafe "sendKey" 
    sendKey :: HWND -> Word8 -> Word8 -> IO ()

--------------------------------------------------------------------------------
-- getName
--------------------------------------------------------------------------------

getName :: HWND -> IO ByteString
getName hwnd = do
    packCString =<< getName' hwnd

foreign import ccall unsafe "getName" 
    getName' :: HWND -> IO CString

--------------------------------------------------------------------------------
-- getClass
--------------------------------------------------------------------------------

getClass :: HWND -> IO ByteString
getClass hwnd = do
    packCString =<< getClass' hwnd

foreign import ccall unsafe "getClass" 
    getClass' :: HWND -> IO CString
