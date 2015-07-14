module BlocVoting.Binary where

import Data.Word (Word32)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Binary.Strict.Get (runGet, getWord32be)

word32ToInt :: Word32 -> Int
word32ToInt = fromIntegral

get4ByteInt :: BS.ByteString -> Int -> Int
get4ByteInt bs def = unwrapInt $ runGet getWord32be $ BS.take 4 bs
      where unwrapInt (Left _, _) = def
            unwrapInt (Right w32, _) = word32ToInt w32
