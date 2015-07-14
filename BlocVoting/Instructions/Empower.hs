module BlocVoting.Instructions.Empower where

import qualified Data.ByteString as BS

import qualified BlocVoting.Nulldata as ND
import BlocVoting.Bitcoin.Address
import BlocVoting.Bitcoin.Base58
import BlocVoting.Binary (get4ByteInt)

data OpEmpower = OpEmpower {
    empVotes :: Int
  , empAddress :: BS.ByteString
}
  deriving (Show, Eq)


fromNulldata :: ND.Nulldata -> OpEmpower
fromNulldata (ND.Nulldata bytes senderAddress) = OpEmpower votes addressPretty
  where votes = get4ByteInt (BS.drop 1 bytes) 0
        addressPretty = encodeBase58 $ BS.drop 5 bytes
