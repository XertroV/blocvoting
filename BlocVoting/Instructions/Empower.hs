module BlocVoting.Instructions.Empower where

import qualified Data.ByteString as BS

import qualified BlocVoting.Nulldata as ND
import BlocVoting.Bitcoin.Address
import BlocVoting.Bitcoin.Base58
import BlocVoting.Binary (get4ByteInt)

data OpEmpower = OpEmpower {
    empVotes :: Int
  , empAddress :: BS.ByteString
  , empND :: ND.Nulldata
}
  deriving (Show, Eq)

_isValidNulldata :: ND.Nulldata -> Bool
_isValidNulldata nd@(ND.Nulldata msg sender)
	| BS.length msg /= 30 = False
	| decodeBase58Check (encodeBase58 addrToEmpower) == Nothing = False
	| otherwise = True
	where addrToEmpower = BS.drop 5 msg

fromNulldata :: ND.Nulldata -> Maybe OpEmpower
fromNulldata nd@(ND.Nulldata msg senderAddress) = if _isValidNulldata nd then Just $ OpEmpower votes addressPretty else Nothing
  where votes = get4ByteInt (BS.drop 1 msg) 0
        addressPretty = encodeBase58 $ BS.drop 5 msg
