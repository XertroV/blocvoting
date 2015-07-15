module BlocVoting.Instructions.Create where

import qualified Data.ByteString as BS

import qualified BlocVoting.Nulldata as ND

data OpCreate = OpCreate {
    networkName :: BS.ByteString
  , adminAddress :: BS.ByteString
}
  deriving (Show, Eq)
  
  
_isValidNulldata :: ND.Nulldata -> Bool
_isValidNulldata nd@(ND.Nulldata msg sender)
		| BS.length msg < 1 = False
		| otherwise = True

fromNulldata :: ND.Nulldata -> Maybe OpCreate
fromNulldata nd@(ND.Nulldata script address) = if _isValidNulldata nd then Just $ OpCreate name address else Nothing
  where name = BS.drop 1 script
