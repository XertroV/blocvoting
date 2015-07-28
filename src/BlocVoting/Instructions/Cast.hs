module BlocVoting.Instructions.Cast where

import qualified Data.ByteString as BS

import qualified BlocVoting.Nulldata as ND
import BlocVoting.Bitcoin.Address
import BlocVoting.Bitcoin.Base58
import BlocVoting.Binary (get1ByteInt)
import BlocVoting.Instructions (op_CAST)

data OpCast = OpCast {
    castScalar :: Int
  , castResolution :: BS.ByteString
  , castND :: ND.Nulldata
}
  deriving (Show, Eq)

_isValidNulldata :: ND.Nulldata -> Bool
_isValidNulldata nd@(ND.Nulldata msg sender _ _)
	| BS.head msg /= op_CAST = False
  | BS.length msg < 3 = False
	| otherwise = True

fromNulldata :: ND.Nulldata -> Maybe OpCast
fromNulldata nd@(ND.Nulldata msg _ _ _) = if _isValidNulldata nd then Just $ OpCast cScalar cRes nd else Nothing
  where cScalar = get1ByteInt $ BS.drop 1 msg
        cRes = BS.drop 2 msg
