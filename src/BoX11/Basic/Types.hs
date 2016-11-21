module BoX11.Basic.Types
-- windows types
    ( HWND, HModule, VK
-- boX11 C-types and values
    , Flags, byName, byClass, byNameEx, byClassEx, vk_num, vk_char
    , vk_SHIFT, vk_CONTROL, vk_ALT, vk_WIN, vk_LWIN
    ) where

import Data.Word
import Foreign.C
import Data.Char

type HWND = Word64

type HModule = Word64

type VK = Word8

type Flags = CInt
byName =    0 :: CInt
byClass =   1 :: CInt
byNameEx =  2 :: CInt
byClassEx = 3 :: CInt

vk_num :: Integral a => a -> VK
vk_num n = (fromIntegral $ fromEnum '0') + (fromIntegral n)

vk_char :: Char -> VK
vk_char = fromIntegral . fromEnum . toUpper

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
