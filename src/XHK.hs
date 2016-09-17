import qualified Data.Map as M

import Control.Monad.Trans.Reader

import Graphics.X11
import Graphics.X11.Xlib.Extras (Event)

import Data.Word
import Data.IORef
import GHC.IO (unsafePerformIO)
import Data.Bits

infixl 7 .<. 
word .<. shift = shiftL word shift

infixl 7 .>. 
word .>. shift = shiftR word shift

-- | XEnv
data XEnv = XEnv
    { display   :: Display
    , rootWindow   :: !Window
    , mousePosition :: !(Maybe (Position, Position))
    , currentEvent :: !(Maybe Event)
    }

-- | X reader monad
type X a = ReaderT (Display) IO a


-- | KC stands for Key Combination
-- the 16 first bits represent the keysym or keycode
-- the next 13 bits are the X modifier masks shifted by 16
-- bit 30 indicates whether it's a keycode (0) or a keysym (1)
-- bit 31 indicates whether it's to be triggered by KeyPress (0) or KeyRelease
-- (1)
type KC = Word32

validKC :: KC -> Bool
validKC kc = kc .&. 0xff /= 0

fromKeysym :: Integral a => a -> KC
fromKeysym n = (fromIntegral n) .|. (1 .<. 30)

fromChar :: Char -> KC
fromChar = fromKeysym . fromEnum

fromString :: String -> KC
fromString = fromKeysym . stringToKeysym

fromKeycode :: Integral a => a -> KC
fromKeycode = fromIntegral

getKeycode :: Num a => KC -> a
getKeycode kc = fromIntegral (kc .&. 0xff)

fromState :: Integral a => a -> KC
fromState n = (fromIntegral n) .<. 16

normalizeKC :: Display -> KC -> IO KC
normalizeKC dpy kc = if testBit kc 30 then do
        ks <- keysymToKeycode dpy (fromIntegral kc .&. 0xffff)
        return $ 0xbfff0000 .&. kc .|. (fromIntegral ks)
    else
        return kc

getState :: Num a => KC -> a
getState kc = fromIntegral $ (kc .>. 16) .&. 0x1fff

shift_ :: KC -> KC
shift_ kc = kc .|. (1 .<. 16)

lock_ :: KC -> KC
lock_ kc = kc .|. (1 .<. 17)

ctrl_ :: KC -> KC
ctrl_ kc = kc .|. (1 .<. 18)

mod1_ :: KC -> KC
mod1_ kc = kc .|. (1 .<. 19)

mod2_ :: KC -> KC
mod2_ kc = kc .|. (1 .<. 20)

mod3_ :: KC -> KC
mod3_ kc = kc .|. (1 .<. 21)

mod4_ :: KC -> KC
mod4_ kc = kc .|. (1 .<. 22)

mod5_ :: KC -> KC
mod5_ kc = kc .|. (1 .<. 23)

btn1_ :: KC -> KC
btn1_ kc = kc .|. (1 .<. 24)

btn2_ :: KC -> KC
btn2_ kc = kc .|. (1 .<. 25)

btn3_ :: KC -> KC
btn3_ kc = kc .|. (1 .<. 26)

btn4_ :: KC -> KC
btn4_ kc = kc .|. (1 .<. 27)

btn5_ :: KC -> KC
btn5_ kc = kc .|. (1 .<. 28)

hkMap :: IORef (M.Map KC (IO ()))
hkMap = unsafePerformIO $ newIORef M.empty
