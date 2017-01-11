{-# LANGUAGE OverloadedStrings, ForeignFunctionInterface #-}

module BoX11.Basic 
    ( module BoX11.Basic.Types 
    , forF, traverseF
    , getWins, getWinsBy, getCursorPos, messageBox
    , postKey, postKeyUp
    , sendKey, sendKeyDown
    , sendKeyUp, sendChar, sendKeyChar, sendClick, moveMouse, clickProp, setText, getName
    , getClass, withMods, withPosted, loadLibrary, getProcAddress
    ) where

import BoX11.Basic.Types

import Data.Foldable

import Control.Concurrent
import Control.Monad

import Foreign hiding (void)
import Foreign.C
import Foreign.C.String
import Foreign.Marshal.Array

import Data.Bits
import Data.ByteString as BS
import Data.IORef

import System.IO.Unsafe

import Unsafe.Coerce

import qualified Data.Map as M

forF :: Traversable t => t a -> (a -> IO b) -> IO ()
forF = flip traverseF

traverseF :: Traversable t => (a -> IO b) -> t a -> IO ()
traverseF f = traverse_ (forkIO . void . f)

--------------------------------------------------------------------------------
-- getWins  
--------------------------------------------------------------------------------

getWins :: Flags -> ByteString -> IO [HWND]
getWins flags bs = do
    ptr <- useAsCString bs (getWins' flags)
    peekArray0 0 ptr

foreign import ccall unsafe "getWins" getWins' :: Flags -> CString -> IO (Ptr Word64)

--------------------------------------------------------------------------------
-- getWinsBy  
--------------------------------------------------------------------------------

getWinsBy :: (HWND -> IO Bool) -> IO [Word64]
getWinsBy f = do
    let f' h = do
        ret <- f h
        return $ case ret of
            True -> (1 :: CInt)
            False -> (0 :: CInt)
    f'' <- mkF f'
    ptr <- getWinsBy' f''
    peekArray0 0 ptr
    
foreign import ccall "wrapper" mkF :: (HWND -> IO CInt) -> IO (FunPtr (HWND -> IO CInt))
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
-- postKey
--------------------------------------------------------------------------------

foreign import ccall unsafe "postKey"
    postKey :: VK -> HWND -> IO ()

--
-- postKeyDown
--

foreign import ccall unsafe "postKeyDown"
    postKeyDown :: VK -> HWND -> IO ()

--------------------------------------------------------------------------------
-- postKeyUp
--------------------------------------------------------------------------------

foreign import ccall unsafe "postKeyUp"
    postKeyUp :: VK -> HWND -> IO ()

--------------------------------------------------------------------------------
-- sendKey
--------------------------------------------------------------------------------

foreign import ccall unsafe "sendKey" 
    sendKey :: VK -> HWND -> IO ()

--------------------------------------------------------------------------------
-- sendKeyDown
--------------------------------------------------------------------------------

foreign import ccall unsafe "sendKeyDown"
    sendKeyDown :: VK -> HWND -> IO ()

--------------------------------------------------------------------------------
-- sendKeyUp
--------------------------------------------------------------------------------

foreign import ccall unsafe "sendKeyUp"
    sendKeyUp :: VK -> HWND -> IO ()

--------------------------------------------------------------------------------
-- sendChar
--------------------------------------------------------------------------------

foreign import ccall unsafe "sendChar" 
    sendChar :: Char -> HWND -> IO ()

--------------------------------------------------------------------------------
-- sendKeyChar
--------------------------------------------------------------------------------

foreign import ccall unsafe "sendKeyChar"
    sendKeyChar :: VK -> Char -> HWND -> IO ()

--------------------------------------------------------------------------------
-- sendClick
--------------------------------------------------------------------------------

foreign import ccall unsafe "sendClick"
    sendClick :: VK -> HWND -> IO ()

--------------------------------------------------------------------------------
-- moveMouse 
--------------------------------------------------------------------------------

foreign import ccall unsafe "moveMouse"
    moveMouse :: Double -> Double -> HWND -> IO ()

--------------------------------------------------------------------------------
-- clickProp
--------------------------------------------------------------------------------

foreign import ccall unsafe "clickProp"
    clickProp :: VK -> Double -> Double -> HWND -> IO ()

--------------------------------------------------------------------------------
-- setText
--------------------------------------------------------------------------------

setText :: ByteString -> HWND -> IO ()
setText txt hwnd = useAsCString txt $ flip setText' hwnd 

foreign import ccall safe "setText"
    setText' :: CString -> HWND -> IO ()

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
-- withModifiers
--------------------------------------------------------------------------------

withMods :: [VK] -> HWND -> IO a -> IO a
withMods mods window act = do
    traverse (flip sendKeyDown window) mods
    ret <- act
    traverse (flip sendKeyUp window) mods
    return ret

withPosted :: [VK] -> HWND -> IO a -> IO a
withPosted mods window act = do
    traverse (flip postKeyDown window) mods
    ret <- act
    traverse (flip postKeyUp window) mods
    return ret 
    
--------------------------------------------------------------------------------
-- loadLibrary
--------------------------------------------------------------------------------

loadLibrary :: ByteString -> IO HModule
loadLibrary dllname = do
    lm <- readIORef lib_map
    case M.lookup dllname lm of
        Just handle -> return handle
        Nothing -> do
            handle <- useAsCString dllname c_loadLibrary 
            modifyIORef lib_map $ \m -> M.insert dllname handle m
            return handle

foreign import ccall "loadLibrary"
    c_loadLibrary :: CString -> IO HModule

lib_map :: IORef (M.Map ByteString HModule)
{-# NOINLINE lib_map #-}
lib_map = unsafePerformIO (newIORef mempty)

--------------------------------------------------------------------------------
-- getProcAddress
--------------------------------------------------------------------------------

getProcAddress :: HModule -> ByteString -> IO a
getProcAddress mod funcname = unsafeCoerce <$> useAsCString funcname $ c_getProcAddress mod

foreign import ccall "getProcAddress"
    c_getProcAddress :: HModule -> CString -> IO ()

--------------------------------------------------------------------------------
-- 
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- 
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- test
--------------------------------------------------------------------------------

foreign import ccall safe "test"
    test :: HWND -> IO ()


--------------------------------------------------------------------------------
-- misc utility stuff
--------------------------------------------------------------------------------

-- foreign import ccall "dynamic"
    -- mkFun :: FunPtr (a -> b) -> (a -> b)
