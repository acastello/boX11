module BoX11.X
    -- reexports
    ( module BoX11.Basic.Types
    , transmit, transmitS, broadcast, broadcastS
    , sendChar, getWins, getWinsBy, getCursorPos 
    , sendKeyDown, sendKeyUp, sendKeyChar, clickWins, portKM)
    where

import XHotkey
import qualified BoX11.Basic as B
import BoX11.Basic.Types 
import Graphics.X11

import Data.Word
import Data.Bits
import Data.Array
import Data.ByteString hiding (foldl)
import Data.Foldable
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Monad.State
import Control.Monad.Reader

getWins :: Flags -> ByteString -> X [HWND]
getWins flags str = liftIO $ B.getWins flags str 

getWinsBy :: (HWND -> IO Bool) -> X [HWND]
getWinsBy = liftIO . B.getWinsBy

getCursorPos :: X (Word, Word)
getCursorPos = liftIO B.getCursorPos

withKM :: KM -> ([VK] -> VK -> X a) -> X (X a)
withKM km f = do
    (mods, vk) <- portKM km
    return $ f mods vk


transmit :: HWND -> X ()
transmit w = do
    km <- askKM
    (mods, vk) <- portKM km
    case km of
        KM _ _ (MButton _) -> clickWin_ mods vk w
        _ -> sendKeyChar_ mods vk w

transmitS :: HWND -> X ()
transmitS w = do
    km <- askKM 
    (mods, vk) <- portKM km
    case km of 
        KM _ _ (MButton _) -> clickWin_ mods vk w
        KM False _ _ -> do
            io $ B.withMods mods w $ B.sendKeyDown vk w 
            sendChar w
        KM True _ _ -> io $ do
            B.sendKeyUp vk w 

broadcast :: Traversable t => t HWND -> X ()
broadcast ws = do
    km <- askKM
    (mods, vk) <- portKM km
    case km of
        KM _ _ (MButton _) -> clickWins vk ws
        _ -> traverse_ (sendKeyChar_ mods vk) ws

broadcastS :: Traversable t => t HWND -> X ()
broadcastS ws = do
    km <- askKM
    (mods, vk) <- portKM km
    case km of
        KM _ _ (MButton _) -> clickWins vk ws
        KM False _ _ -> do
            (_, str) <- askKeysym
            io $ flip traverse_ ws $ \w -> B.withMods mods w $ do 
                B.sendKeyDown vk w 
                case str of
                    (c:_) -> B.sendChar c w
                    _ -> return ()
        KM True _ _ -> io $ do
            traverse_ (B.sendKeyUp vk) ws 
        
sendKeyDown :: HWND -> X ()
sendKeyDown w = do
    km <- askKM
    (mods, vk) <- portKM km
    when (not$ keyUp km) $ io $ B.withMods mods w (B.sendKeyDown vk w)
    return ()
    
sendKeyUp :: HWND -> X ()
sendKeyUp w = do
    km <- askKM
    (mods, vk) <- portKM km
    when (keyUp km) $ io $ B.withMods mods w (B.sendKeyUp vk w)
    return ()
    
sendChar :: HWND -> X ()
sendChar w = do
    XEnv { currentEvent = ev } <- ask
    (_, c:_) <- io $ lookupString (asKeyEvent ev)
    io $ B.sendChar c w

sendKeyChar :: HWND -> X ()
sendKeyChar w = do
    (mods, vk) <- portKM =<< askKM
    sendKeyChar_ mods vk w
    return ()

sendKeyChar_ :: [VK] -> VK -> HWND -> X ()
sendKeyChar_ mods vk w = do
    (_, s) <- askKeysym 
    case s of 
        (c:_) -> io $ B.withMods mods w $ B.sendKeyChar vk c w
        _ -> return () 
    
pressWins :: Traversable t => VK -> t HWND -> X ()
pressWins k ws = traverse_ (liftIO . B.sendKey k) ws

clickWin :: HWND -> X ()
clickWin w = do
    (mods, vk) <- portKM =<< askKM
    when (vk <= 9) $ clickWin_ mods vk w
        
clickWin_ :: [VK] -> VK -> HWND -> X ()
clickWin_ mods k w = do
    (xp, yp) <- pointerProp
    io $ B.withMods mods w $ B.clickProp k xp yp w

clickWins :: Traversable t => VK -> t HWND -> X ()
clickWins k wins = inCurrentPos $ do
    (xp, yp) <- pointerProp
    traverse (liftIO . B.clickProp k xp yp) wins
    -- io $ threadDelay 80000
    return ()

vkMap :: Array KeyCode VK
vkMap = listArray (minBound, maxBound) $ (fromIntegral . fromEnum) <$>
   -- 0x0                                      0x8 Esc, Numbers...
    [ 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00, 0x00,0x1B,0x31,0x32,0x33,0x34,0x35,0x36
   -- 0x10                          Back Tab   0x18 Q-I
    , 0x37,0x38,0x39,0x30,0xBD,0xBB,0x08,0x09, 0x51,0x57,0x45,0x52,0x54,0x59,0x55,0x49
   -- 0x20 O,P, Oem4,Oem6,Ret, LCtr,A,   S     0x28 D-L                           Oem1
    , 0x4f,0x50,0xDB,0xDD,0x0D,0xA2,0x41,0x53, 0x44,0x46,0x47,0x48,0x4a,0x4b,0x4c,0xBA
   -- 0x30                                     0x38
    , 0xDE,0xC0,0xA0,0xDC,0x5A,0x58,0x43,0x56, 0x42,0x4E,0x4D,0xBC,0xBE,0xBF,0xA1,0x6A
   -- 0x40                                     0x48
    , 0xA4,0x20,0x14,0x70,0x71,0x72,0x73,0x74, 0x75,0x76,0x77,0x78,0x79,0x90,0x91,0x67
   -- 0x50                                     0x58                ISO3?
    , 0x68,0x69,0x6D,0x64,0x65,0x66,0x6B,0x61, 0x62,0x63,0x60,0x6E,0x00,0x00,0xE2,0x7A
   -- 0x60                                     0x68                OemReset? Linefeed?
    , 0x7B,0x00,0x00,0x00,0x00,0x00,0x00,0x00, 0x0D,0xA3,0x6F,0x2A,0xE9,0x00,0x24,0x26
   -- 0x70                                     0x78                     KP_EQ?
    , 0x21,0x25,0x27,0x23,0x28,0x22,0x2D,0x2E, 0x00,0xAD,0xAE,0xAF,0x00,0x00,0x00,0x13
   -- 0x80                                     0x88
    , 0xB6,0x6E,0x15,0x19,0x00,0x5B,0x5C,0x5D, 0x03,0x00,0x00,0x00,0x00,0xF2,0x00,0x00
   -- 0x90                Calc?                0x98 XF86 Stuff
    , 0x00,0x00,0x2F,0x00,0x00,0x5F,0x00,0x00, 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
   -- 0xA0                                     0xA8
    , 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00, 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    , 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00, 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    , 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00, 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    , 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00, 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    , 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00, 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00
    , 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00, 0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00 ]

modsToVK :: Modifier -> [VK]
modsToVK m = mconcat
    [ if testBit m 0 then [vk_SHIFT] else []
    , if testBit m 2 then [vk_CONTROL] else []
    , if testBit m 3 then [vk_ALT] else []
    , if testBit m 6 then [vk_WIN] else []
    ]
    
portKM :: KM -> X ([VK], VK)
portKM (KM u st (KSym ks)) = do
    XEnv { display = dpy } <- ask
    kc <- io $ keysymToKeycode dpy ks
    return (modsToVK st, vkMap!kc)
portKM (KM u st (KCode kc)) = do
    XEnv { display = dpy } <- ask
    return (modsToVK st, vkMap!kc)
portKM (KM u st (MButton b)) =
    return (modsToVK st, port' b)
    where 
        port' b 
            | b <= 3 = fromIntegral b   -- left, middle and right click
            | b <= 7 = fromIntegral (b+2)   -- scroll up/down/left/right
            | otherwise = fromIntegral (b-4)    -- extra buttons

-- sendKey :: Foldable t => KM -> t HWND -> X ()
-- sendKey k ws = do
    -- <F5>


