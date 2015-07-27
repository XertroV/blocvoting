module BlocVoting.Instructions where

import qualified Data.ByteString as BS
import Data.Word (Word8)


import BlocVoting.Nulldata

op_NULL = 0x00 :: Word8

op_CREATE = 0x01 :: Word8
op_MOD_RES = 0x02 :: Word8
op_EMPOWER = 0x03 :: Word8

op_CAST = 0x10 :: Word8
op_DELEGATE = 0x11 :: Word8

op_TRANSFER = 0x20 :: Word8
op_ENABLE_TFER = 0x21 :: Word8
op_DISABLE_TFER = 0x22 :: Word8

operationIs :: Word8 -> Nulldata -> Bool
operationIs op nd = BS.head (ndScript nd) == op
