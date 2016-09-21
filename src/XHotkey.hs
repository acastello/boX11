{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module XHotkey.Types.X where

import XHotkey.Types.KM

import qualified Data.Map as M

import Control.Monad.Reader
import Control.Monad.State

import Graphics.X11
import Graphics.X11.Xlib.Extras (Event)

import Data.Word
import Data.IORef
import GHC.IO (unsafePerformIO)
import Data.Bits
import Numeric (showHex)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import qualified Text.Read as T
import Text.Read.Lex (numberToInteger)

infixl 7 .<. 
word .<. shift = shiftL word shift

infixl 7 .>. 
word .>. shift = shiftR word shift

-- | XEnv
data XEnv = XEnv
    { display   :: Display
    , rootWindow   :: !Window
    , currentEvent :: !(Maybe XEventPtr)
    , mousePosition :: !(Maybe (Position, Position))
    }

data XControl = XControl
    { hkMap :: M.Map KC (X ())
    , exitScheduled :: Bool
    }

-- | X reader monad
newtype X a = X (ReaderT XEnv (StateT XControl IO) a)
    deriving (Functor, Applicative, Monad, MonadReader XEnv, MonadState XControl, MonadIO)

runX :: (X a) -> XEnv -> XControl -> IO (a, XControl)
runX (X a) env control = runStateT (runReaderT a env) control

-- | KC stands for Key Combination
data KC = 
        KCcode
    { kc_onrelease  :: Bool
    , kc_state      :: Modifier
    , kc_keycode    :: KeyCode
    }
      | KCsym
    { kc_onrelease  :: Bool
    , kc_state      :: Modifier
    , kc_keysym     :: KeySym
    }
      | KCmouse
    { kc_onrelease  :: Bool
    , kc_state      :: Modifier
    , kc_button     :: Button
    }
    deriving (Eq)

instance Show KC where
    show kc =
        let u = kc_onrelease kc
            s = foldMap state' (kc_stateToList $ kc_state kc)
        in s ++ (show' kc) ++ (if u then " Up" else "")
        where show' (KCcode _ _ i) = "0x" ++ showHex i ""
              show' (KCsym _ _ c) = keysymToString c
              show' (KCmouse _ _ m) = "mouse" ++ (show m)
              state' kc = fromMaybe "" $ lookup kc
                [ (shiftMask, "Shift-"), (lockMask, "Caps-"), (controlMask, "Ctrl-")
                , (mod1Mask, "Mod1-"), (mod2Mask, "Mod2-"), (mod3Mask, "Mod3-")
                , (mod4Mask, "Mod4-"), (mod5Mask, "Mod5-"), (button1Mask, "Btn1-")
                , (button2Mask, "Btn2-"), (button3Mask, "Btn3-")
                , (button4Mask, "Btn4-"), (button5Mask, "Btn5-") ]
    
instance Read KC where
        readPrec = T.parens $ do 
                    s <- readState 0

                    kc <- T.choice 
                        [ do
                                str <- T.choice
                                       [do 
                                            T.Ident str' <- T.lexP
                                            return str'
                                       ,do
                                            n' <- T.readPrec :: T.ReadPrec Integer
                                            return (show n')]
                                let ks = stringToKeysym str
                                if ks == 0 then
                                    fail "invalid keysym string"
                                else
                                    return (\t -> KCsym t s ks)
                        , do
                                lit "c"
                                c <- T.get
                                return (\t -> KCsym t s (fromIntegral $ fromEnum c))
                        , do
                                lit "k"
                                n <- T.readPrec :: T.ReadPrec Word8
                                return (\t -> KCcode t s n)
                        , do
                                lit "m" T.+++ lit "mouse"
                                n <- T.readPrec
                                return (\t -> KCmouse t s n) ]

                    onrel <- T.choice
                        [ do
                            '\'' <- T.get
                            return True
                        , do
                            return False ]
                    return $ kc onrel
            where
                readState st = T.choice
                    ((\(k,s) -> do
                        lit k
                        readState (st .|. s))
                    <$> modMap)
                    T.+++ do
                        return st
                modMap = foldMap (uncurry zip) $ fmap repeat <$>
                    [ (["S-", "Shift-", "shift "], shiftMask)
                    , (["Caps-", "CapsLock "], lockMask)
                    , (["C-", "Ctrl-", "ctrl ", "control "], controlMask)
                    , (["mod1-", "mod1 ", "M-", "meta ", "A-", "Alt-", "alt "], mod1Mask)
                    , (["mod2-", "mod2 ", "Num-", "NumLock "], mod2Mask)
                    , (["mod3-", "mod3 ", "Scroll-", "ScrollLock "], mod3Mask)
                    , (["mod4-", "mod4 ", "Win-", "Win ", "Cmd-", "Cmd ", "Super "], mod4Mask)
                    , (["mod5-", "mod5 "], mod5Mask)
                    , (["btn1-", "button1 "], button1Mask)
                    , (["btn2-", "button2 "], button2Mask)
                    , (["btn3-", "button3 "], button3Mask)
                    , (["btn4-", "button4 "], button4Mask)
                    , (["btn5-", "button5 "], button5Mask) ]
                lit = mapM (\c -> T.get >>= \c' -> 
                    if toLower c' == toLower c then return c' else fail "")

instance Ord KC where
    KCcode a1 b1 c1 `compare` KCcode a2 b2 c2 = lexic a1 b1 c1 a2 b2 c2
    KCsym  a1 b1 c1 `compare` KCsym  a2 b2 c2 = lexic a1 b1 c1 a2 b2 c2
    KCmouse a1 b1 c1 `compare` KCmouse a2 b2 c2 = lexic a1 b1 c1 a2 b2 c2
    KCcode _ _ _ `compare` _ = GT
    KCsym _ _ _ `compare` _ = GT

lexic :: (Ord a, Ord b, Ord c) => a -> b -> c -> a -> b -> c -> Ordering
lexic a1 b1 c1 a2 b2 c2 = 
    compare c1 c2 `mappend` compare b1 b2 `mappend` compare a1 a2

kc_state_set :: KC -> Modifier -> KC
kc_state_set (KCcode u _ k) st = KCcode u st k
kc_state_set (KCsym u _ c) st = KCsym u st c
kc_state_set (KCmouse u _ m) st = KCmouse u st m

kc_onrelease_set :: KC -> Bool -> KC
kc_onrelease_set (KCcode _ st k) u = KCcode u st k
kc_onrelease_set (KCsym _ st c) u = KCsym u st c
kc_onrelease_set (KCmouse _ st m) u = KCmouse u st m

kc_stateToList :: Modifier -> [Modifier]
kc_stateToList s = [s .&. (1 .<. i) | i <- [0..12], testBit s i]

kc_int :: KC -> Int
kc_int (KCcode _ _ i) = fromEnum i
kc_int (KCsym _ _ i) = fromEnum i
kc_int (KCmouse _ _ i) = fromEnum i

normalizeKC :: Display -> KC -> IO (Maybe KC)
normalizeKC dpy (KCsym onRelease state ks) = do
    kc <- keysymToKeycode dpy ks
    if kc == 0 then
        return Nothing
    else
        return $ Just (KCcode onRelease state kc)
normalizeKC _ kc = return (Just kc)

