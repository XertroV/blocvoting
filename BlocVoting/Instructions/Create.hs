module BlocVoting.Instructions.Create where

import qualified Data.ByteString as BS

import qualified BlocVoting.Nulldata as ND

data OpCreate = OpCreate {
    networkName :: BS.ByteString
  , adminAddress :: BS.ByteString
}
  deriving (Show, Eq)

fromNulldata :: ND.Nulldata -> OpCreate
fromNulldata (ND.Nulldata script address) = OpCreate name address
  where name = BS.drop 1 script
