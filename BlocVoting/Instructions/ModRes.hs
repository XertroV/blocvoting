module BlocVoting.Instructions.ModRes where

import qualified Data.ByteString as BS

import qualified BlocVoting.Nulldata as ND
import BlocVoting.Bitcoin.Address
import BlocVoting.Bitcoin.Base58
import BlocVoting.Binary (get4ByteInt, get1ByteInt)

data OpModRes = OpModRes {
    mrCategories :: Int
  , mrEndTimestamp :: Int
  , mrResolution :: BS.ByteString
  , mrUrl :: BS.ByteString
  , mrND :: ND.Nulldata
}
  deriving (Show, Eq)

_isValidNulldata :: ND.Nulldata -> Bool
_isValidNulldata nd@(ND.Nulldata msg sender)
    | BS.length msg > 40 = False
    | otherwise = True

fromNulldata :: ND.Nulldata -> Maybe OpModRes
fromNulldata nd@(ND.Nulldata msg senderAddress) = if _isValidNulldata nd then Just $ OpModRes cats endT resolution url else Nothing
  where cats = get1ByteInt $ BS.drop 1 msg
        endT = get4ByteInt (BS.drop 2 msg) 0
        resL = get1ByteInt $ BS.drop 6 msg
        resolution = BS.take resL $ BS.drop 7 msg
        lastChunk = BS.drop (resL + 7) msg
        urlL = get1ByteInt lastChunk
        url = BS.take urlL $ BS.drop 1 lastChunk
