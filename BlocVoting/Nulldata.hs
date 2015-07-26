module BlocVoting.Nulldata where

import qualified Data.ByteString as BS

data Nulldata = Nulldata {
    ndScript :: BS.ByteString
  , ndAddress :: BS.ByteString
  , ndTimestamp :: Int
  , ndHeight :: Int
}
  deriving (Show, Eq)

modNulldataScript :: BS.ByteString -> Nulldata -> Nulldata
modNulldataScript newScript (Nulldata _ address ts h) = Nulldata newScript address ts h
