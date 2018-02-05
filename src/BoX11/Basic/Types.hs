module BoX11.Basic.Types
-- windows types
    ( HWND, HModule, VK, Key
-- boX11 C-types and values
    , Flags, byName, byClass, byNameEx, byClassEx
    , vk_num, vk_char, vk_f, vk_numpad, vk_multiply, vk_add, vk_separator
    , vk_subtract, vk_decimal, vk_divide, vk_esc, vk_return, vk_back, vk_tab
    , vk_insert , vk_delete, vk_home, vk_end, vk_prior, vk_next, vk_up, vk_down
    , vk_left , vk_right , vk_SHIFT, vk_CONTROL, vk_ALT, vk_WIN, vk_LWIN, vk_TAB
    , VKt(..), vkt
    ) where

import Control.Monad

import Data.Word
import Data.Char
import Data.Function

import Foreign.C

import Text.Read (readPrec, ReadPrec)
import qualified Text.Read as T
import Text.ParserCombinators.ReadP (skipSpaces)

type HWND = Word64

type HModule = Word64

type Key = Word32
type VK = Word32

type Flags = CInt
byName =    0 :: CInt
byClass =   1 :: CInt
byNameEx =  2 :: CInt
byClassEx = 3 :: CInt

vk_num :: Integral a => a -> VK
vk_num n = (fromIntegral $ fromEnum '0') + (fromIntegral n)

vk_char :: Char -> VK
vk_char = fromIntegral . fromEnum . toUpper

vk_f :: Integral a => a -> VK
vk_f f = 0x6F + fromIntegral f

vk_numpad :: Integral a => a -> VK
vk_numpad n = 0x60 + fromIntegral n

vk_multiply :: VK
vk_multiply = 0x6A 
vk_add :: VK
vk_add = 0x6B 
vk_separator = 0x6C :: VK 
vk_subtract = 0x6D :: VK 
vk_decimal = 0x6E :: VK 
vk_divide = 0x6F :: VK 
vk_esc = 0x1B :: VK 
vk_return = 0x0D :: VK 
vk_back = 0x08 :: VK
vk_comma = 0xBC :: VK
vk_period = 0xBE :: VK
vk_minus = 0xBD :: VK
vk_tab = vk_TAB :: VK
vk_insert = 0x2D :: VK
vk_delete = 0x2E :: VK
vk_home = 0x24 :: VK
vk_end = 0x23 :: VK
vk_prior = 0x21 :: VK
vk_next = 0x22 :: VK
vk_left = 0x25 :: VK
vk_right = 0x27 :: VK
vk_up = 0x26 :: VK
vk_down = 0x28 :: VK

vk_SHIFT :: VK
vk_SHIFT = 0x10

vk_CONTROL :: VK
vk_CONTROL = 0x11

vk_ALT :: VK
vk_ALT = 0x12

vk_WIN :: VK
vk_WIN = vk_LWIN

vk_LWIN :: VK
vk_LWIN = 0x5B

vk_TAB :: VK
vk_TAB = 0x09

data VKt =
    VKChar Char
  | VKNum Int
  | VKF Int
  | VKShift
  | VKCtrl
  | VKAlt
  | VKWin
  | VKEsc
  | VKRet
  | VKBac
  | VKComma
  | VKPeriod
  | VKMinus
  | VKTab
  | VKIns 
  | VKDel
  | VKHom
  | VKEnd
  | VKPri
  | VKNex
-- Arrows
  | VKLef
  | VKRig
  | VKUp
  | VKDow
-- Keypad buttons
  | VKPad Int
  | VKDiv
  | VKMul
  | VKSub
  | VKAdd
  | VKSep
  | VKDec
  | VKLit Word32
  deriving Show

instance Eq VKt where
    (==) = (==) `on` vkt

instance Ord VKt where
    compare = compare `on` vkt

vkt :: VKt -> VK
vkt vk = case vk of
    VKChar c -> vk_char c
    VKNum n -> vk_num n
    VKF n -> vk_f n
    VKPad n -> vk_numpad n
    VKShift -> vk_SHIFT
    VKCtrl  -> vk_CONTROL
    VKAlt   -> vk_ALT
    VKWin   -> vk_WIN
    VKEsc   -> vk_esc
    VKRet   -> vk_return
    VKBac   -> vk_back
    VKComma -> vk_comma
    VKPeriod -> vk_period
    VKMinus -> vk_minus
    VKTab -> vk_tab
    VKIns -> vk_insert
    VKDel -> vk_delete
    VKHom -> vk_home
    VKEnd -> vk_end
    VKPri -> vk_prior
    VKNex -> vk_next
    VKLef -> vk_left
    VKRig -> vk_right
    VKUp  -> vk_up
    VKDow -> vk_down
    VKDiv -> vk_divide
    VKMul -> vk_multiply
    VKSub -> vk_subtract
    VKAdd -> vk_add
    VKSep -> vk_separator
    VKDec -> vk_decimal
    VKLit n -> n

instance Read VKt where
    readPrec = T.parens $ T.choice $
      fmap (\(t, s) -> ws >> getanystr s >> return t) 
      [ (VKShift, ["shift"])
      , (VKCtrl, ["ctrl", "control"])
      , (VKAlt, ["alt", "meta"])
      , (VKWin, ["win", "windows"])
      , (VKEsc, ["esc", "escape"])
      , (VKRet, ["ret", "return", "enter"])
      , (VKBac, ["back", "backspace"])
      , (VKComma, ["comma"])
      , (VKPeriod, ["period"])
      , (VKMinus, ["minus", "dash"])
      , (VKTab, ["tab"])
      , (VKIns, ["ins", "insert"])
      , (VKDel, ["del", "delete"])
      , (VKHom, ["home"])
      , (VKEnd, ["end"])
      , (VKPri, ["prior", "prior_page", "prev"])
      , (VKNex, ["next", "next_page"])
      , (VKLef, ["left", "arrow_left"])
      , (VKRig, ["right", "arrow_right"])
      , (VKUp,  ["up", "arrow_up"])
      , (VKDow, ["down", "arrow_down"])
      , (VKDiv, (++) <$> ["","keypad","kp_"] <*> ["div", "divide"])
      , (VKMul, (++) <$> ["","keypad","kp_"] <*> ["mul", "mult", "multiply"])
      , (VKSub, (++) <$> ["","keypad","kp_"] <*> ["sub", "subtract"])
      , (VKAdd, (++) <$> ["","keypad","kp_"] <*> ["add", "plus"])
      , (VKSep, (++) <$> ["","keypad","kp_"] <*> ["sep", "Separator"])
      , (VKDec, (++) <$> ["","keypad","kp_"] <*> ["Dec", "Decimal"])
      ] ++ 
      [ do
          ws
          getanystr ["kp", "kp_", "keypad"]
          n <- readPrec
          when (n < 0 || n > 9) T.pfail
          return $ VKPad n
      , do
          ws
          getanystr ["space"]
          return $ VKChar ' '
      , do
          c <- readPrec
          unless (elem c ([' '] ++ ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']))
              T.pfail
          return $ VKChar $ toUpper c
      , do 
          n <- readPrec
          when (n < 0 || n > 9) T.pfail
          return $ VKNum n
      , do
          ws
          getanystr ["F"]
          n <- readPrec
          when (n < 0 || n > 24) T.pfail
          return $ VKF n
      , do
          ws
          c <- T.get
          unless (isAlpha c) T.pfail
          return $ VKChar (toUpper c)
      , do
          ws
          getanystr ["#"]
          n <- readPrec
          return $ VKLit n
      , do
          n <- readPrec
          unless (n < 0 || n > 9) T.pfail
          return $ VKLit n
      ]

-- utils

ws :: ReadPrec ()
ws = T.lift skipSpaces

getstring :: String -> ReadPrec ()
getstring = mapM_ (\c -> T.get >>= \c' -> when (toLower c /= toLower c') T.pfail)

getanystr :: [String] -> ReadPrec ()
getanystr xs = T.choice (getstring <$> xs)
