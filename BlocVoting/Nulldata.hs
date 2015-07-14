module BlocVoting.Nulldata where

import qualified Data.ByteString as BS

data Nulldata = Nulldata {
    ndScript :: BS.ByteString
  , ndAddress :: BS.ByteString
}
  deriving (Show, Eq)

modNulldataScript :: BS.ByteString -> Nulldata -> Nulldata
modNulldataScript newScript (Nulldata _ address) = Nulldata newScript address
