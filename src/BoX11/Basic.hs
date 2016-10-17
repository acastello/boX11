{-# LANGUAGE OverloadedStrings, ForeignFunctionInterface #-}

module BoX11.Basic where

import Foreign
import Foreign.C
import Foreign.C.String
import Foreign.Marshal.Array

import Data.Bits
import Data.ByteString as BS

type HWND = Word64

type Flags = CInt
byName =    0 :: CInt
byClass =   1 :: CInt
byNameEx =  2 :: CInt
byClassEx = 3 :: CInt

--------------------------------------------------------------------------------
-- getWins  
--------------------------------------------------------------------------------

getWins :: Flags -> ByteString -> IO [Word64]
getWins flags bs = do
    ptr <- useAsCString bs (getWins' flags)
    peekArray0 0 ptr

foreign import ccall unsafe "getWins" getWins' :: Flags -> CString -> IO (Ptr Word64)

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
foreign import ccall safe "getWinsBy" 
    getWinsBy' :: FunPtr (Word64 -> IO CInt) -> IO (Ptr Word64)

--------------------------------------------------------------------------------
-- getCursorPos  
--------------------------------------------------------------------------------

getCursorPos :: IO (Word, Word)
getCursorPos = do
    pos <- getCursorPos'
    return (fromIntegral $ pos .&. 0xffffffff, fromIntegral $ shiftR pos 32)

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

foreign import ccall safe "sendKey" 
    sendKey :: Word8 -> HWND -> IO ()

--------------------------------------------------------------------------------
-- sendKeyDown
--------------------------------------------------------------------------------

foreign import ccall safe "sendKeyDown"
    sendKeyDown :: Word8 -> HWND -> IO ()

--------------------------------------------------------------------------------
-- sendKeyUp
--------------------------------------------------------------------------------

foreign import ccall safe "sendKeyUp"
    sendKeyUp :: Word8 -> HWND -> IO ()

--------------------------------------------------------------------------------
-- sendChar
--------------------------------------------------------------------------------

foreign import ccall safe "sendChar" 
    sendChar :: Word8 -> HWND -> IO ()

--------------------------------------------------------------------------------
-- sendClick
--------------------------------------------------------------------------------

foreign import ccall safe "sendClick"
    sendClick :: Word32 -> HWND -> IO ()

--------------------------------------------------------------------------------
-- moveMouse 
--------------------------------------------------------------------------------

foreign import ccall unsafe "moveMouse"
    moveMouse :: Double -> Double -> HWND -> IO ()

--------------------------------------------------------------------------------
-- clickProp
--------------------------------------------------------------------------------

foreign import ccall unsafe "clickProp"
    clickProp :: Word32 -> Double -> Double -> HWND -> IO ()

--------------------------------------------------------------------------------
-- sendText
--------------------------------------------------------------------------------

setText :: ByteString -> HWND -> IO ()
setText txt hwnd = useAsCString txt $ flip sendText' hwnd 

foreign import ccall safe "sendText"
    sendText' :: CString -> HWND -> IO ()

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

--------------------------------------------------------------------------------
-- 
--------------------------------------------------------------------------------


--------------------------------------------------------------------------------
-- test
--------------------------------------------------------------------------------

foreign import ccall safe "test"
    test :: HWND -> IO ()

