module BlocVoting.Instructions.Delegate where

import qualified Data.ByteString as BS

import qualified BlocVoting.Nulldata as ND
import BlocVoting.Bitcoin.Address
import BlocVoting.Bitcoin.Base58
import BlocVoting.Binary (get1ByteInt)
import BlocVoting.Instructions (op_DELEGATE)

data OpDelegate = OpDelegate {
    dlgCats :: Int
  , dlgAddr :: BS.ByteString
  , dlgND :: ND.Nulldata
}
  deriving (Show, Eq)

_isValidNulldata :: ND.Nulldata -> Bool
_isValidNulldata nd@(ND.Nulldata msg sender _ _)
  | BS.head msg /= op_DELEGATE = False
  | BS.length msg /= 27 = False
  -- | Address (BS.drop 2 msg)
  | otherwise = True

fromNulldata :: ND.Nulldata -> Maybe OpDelegate
fromNulldata nd@(ND.Nulldata msg senderAddress _ _)
    | _isValidNulldata nd = Just $ OpDelegate dCats dAddr nd
    | otherwise = Nothing
  where dCats = get1ByteInt $ BS.drop 1 msg
        dAddr = BS.drop 2 msg
