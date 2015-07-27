module BlocVoting.Binary where

import Data.Word (Word32)
import qualified Data.ByteString as BS
import Data.Binary.Strict.Get (runGet, getWord32be)

word32ToInt :: Word32 -> Int
word32ToInt = fromIntegral

unwrapInt :: (Integral b) => Int -> (Either a b, c) -> Int
unwrapInt def (Left _, _) = def
unwrapInt _ (Right w, _) = fromIntegral w

get4ByteInt :: BS.ByteString -> Int -> Int
get4ByteInt bs def = unwrapInt def $ runGet getWord32be bs

get1ByteInt :: BS.ByteString -> Int
get1ByteInt bs = fromIntegral $ BS.head bs
