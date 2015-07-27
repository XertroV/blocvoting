module BlocVoting.Tally.Transfer where

import qualified Data.ByteString as BS

import BlocVoting.Tally.Voter

data Transfer = Transfer {
    fromVoter :: Voter
  , afterTime :: Integer
  , newAddress :: BS.ByteString
}
  deriving (Show, Eq)
