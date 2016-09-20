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

-- data XEnv = XEnv
    -- { 

-- | X reader monad
-- newtype X a = X (ReaderT XEnv
type X a = ReaderT XEnv IO a


-- | KC stands for Key Combination
-- the 16 first bits represent the keysym or keycode
-- the next 13 bits are the X modifier masks shifted by 16
-- bit 30 indicates whether it's a keycode (0) or a keysym (1)
-- bit 31 indicates whether it's to be triggered by KeyPress (0) or KeyRelease
-- (1)
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
                    [ (["S-", "shift "], shiftMask)
                    , (["Caps-", "CapsLock "], lockMask)
                    , (["C-", "ctrl ", "control "], controlMask)
                    , (["mod1-", "mod1 ", "M-", "meta ", "A-", "alt"], mod1Mask)
                    , (["mod2-", "mod2 ", "Num-", "NumLock "], mod2Mask)
                    , (["mod3-", "mod3 ", "Scroll-", "ScrollLock "], mod3Mask)
                    , (["mod4-", "mod4 ", "Win-", "Win ", "Cmd-", "Cmd ", "Super "], mod4Mask)
                    , (["mod5-", "mod5 "], mod5Mask)
                    , (["btn1-", "button1 "], button1Mask)
                    , (["btn2-", "button2 "], button2Mask)
                    , (["btn3-", "button3 "], button3Mask)
                    , (["btn4-", "button4 "], button4Mask)
                    , (["btn5-", "button5 "], button5Mask)
                    ]
                    -- [ ("S-", shiftMask), ("shift ", shiftMask), ("Caps-", lockMask)
                    -- , ("CapsLock ", lockMask), ("C-", controlMask), ("ctrl ", controlMask)
                    -- , ("control ", controlMask), ("mod1-", mod1Mask), ("mod1 ", mod1Mask)
                    -- , ("M-", mod1Mask) , ("meta ", mod1Mask), ("A-", mod1Mask)
                    -- , ("alt ", mod1Mask) , ("mod2-", mod2Mask), ("mod2 ", mod2Mask)
                    -- , ("Num-", mod2Mask), ("NumLock ", mod2Mask), ("mod3-", mod3Mask)
                    -- , ("mod3 ", mod3Mask), ("Scroll-", mod3Mask), ("ScrollLock", mod3Mask)
                    -- , ("mod4-", mod4Mask), ("mod4 ", mod4Mask), ("Win-", mod4Mask)
                    -- , ("Win ", mod4Mask), ("Cmd-", mod4Mask), ("Cmd ", mod4Mask)
                    -- , ("Super ", mod4Mask), ("mod5-", mod5Mask), ("mod5 ", mod5Mask) ]
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

kc_setstate :: KC -> Modifier -> KC
kc_setstate (KCcode u _ k) st = KCcode u st k
kc_setstate (KCsym u _ c) st = KCsym u st c
kc_setstate (KCmouse u _ m) st = KCmouse u st m

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


hkMap :: IORef (M.Map KC (IO ()))
hkMap = unsafePerformIO $ newIORef M.empty
