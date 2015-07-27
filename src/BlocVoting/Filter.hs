module BlocVoting.Filter where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C

import BlocVoting.Nulldata

filterPrefix :: BS.ByteString -> [Nulldata] -> [Nulldata]
filterPrefix prefix = map (\nd -> modNulldataScript (BS.drop len $ ndScript nd) nd) . filter ((== prefix) . BS.take len . ndScript)
  where len = BS.length prefix

filterNVB :: [Nulldata] -> [Nulldata]
filterNVB = filterPrefix (C.pack "NVB")
