module BoX11.Basic.Types
-- windows types
    ( HWND, HModule, VK, Key
-- boX11 C-types and values
    , Flags, byName, byClass, byNameEx, byClassEx
    , vk_num, vk_char, vk_f, vk_numpad, vk_multiply, vk_add, vk_separator
    , vk_subtract, vk_decimal, vk_divide, vk_esc, vk_return
    , vk_SHIFT, vk_CONTROL, vk_ALT, vk_WIN, vk_LWIN, vk_TAB
    ) where

import Data.Word
import Foreign.C
import Data.Char

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
vk_numpad n = 0x5F + fromIntegral n

vk_multiply = 0x6A :: VK

vk_add = 0x6B :: VK

vk_separator = 0x6C :: VK

vk_subtract = 0x6D :: VK

vk_decimal = 0x6E :: VK

vk_divide = 0x6F :: VK

vk_esc = 0x1B :: VK

vk_return = 0x0D :: VK

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
