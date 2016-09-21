{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module XHotkey.Types where

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
    { hkMap :: M.Map KM (X ())
    , exitScheduled :: Bool
    }

-- | X reader monad
newtype X a = X (ReaderT XEnv (StateT XControl IO) a)
    deriving (Functor, Applicative, Monad, MonadReader XEnv, MonadState XControl, MonadIO)

runX :: (X a) -> XEnv -> XControl -> IO (a, XControl)
runX (X a) env control = runStateT (runReaderT a env) control


data KM = KM 
    { keyUp :: Bool
    , keyModifiers :: Modifier
    , mainKey :: KMitem }
    deriving (Eq)

data KMitem = 
        KCode KeyCode
      | KSym KeySym
      | MButton Button
    deriving (Eq, Ord)

instance Show KMitem where
    show (KCode c) = "0x" ++ showHex c ""
    show (KSym s) = keysymToString s
    show (MButton b) = "mouse" ++ (show b)
    

instance Show KM where
    show (KM up mod k) = 
        let s = foldMap state' (listModifiers mod)
        in s ++ (show k) ++ (if up then " Up" else "")
        where state' kc = fromMaybe "" $ lookup kc
                [ (shiftMask, "Shift-"), (lockMask, "Caps-"), (controlMask, "Ctrl-")
                , (mod1Mask, "Mod1-"), (mod2Mask, "Mod2-"), (mod3Mask, "Mod3-")
                , (mod4Mask, "Mod4-"), (mod5Mask, "Mod5-"), (button1Mask, "Btn1-")
                , (button2Mask, "Btn2-"), (button3Mask, "Btn3-")
                , (button4Mask, "Btn4-"), (button5Mask, "Btn5-") ]

instance Read KM where
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
                                return (KSym ks)
                    , do
                            lit "c"
                            c <- T.get
                            return (KSym $ fromIntegral $ fromEnum c)
                    , do
                            lit "k"
                            n <- T.readPrec
                            return (KCode n)
                    , do
                            lit "m" T.+++ lit "mouse"
                            n <- T.readPrec
                            return (MButton n) ]

                onrel <- T.choice
                    [ do
                        '\'' <- T.get
                        return True
                    , do
                        return False ]
                return $ KM onrel s kc
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

instance Ord KM where
    KM u1 m1 k1 `compare` KM u2 m2 k2 = compare k1 k2 `mappend` compare m1 m2 `mappend` compare u1 u2

listModifiers :: Modifier -> [Modifier]
listModifiers s = [s .&. (1 `shiftL` i) | i <- [0..12], testBit s i]