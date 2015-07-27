module BlocVoting.Tally.NetworkSettings where

import qualified Data.ByteString as BS

data NetworkSettings = NetworkSettings {
    nsAdminAddress :: BS.ByteString
  , nsNetworkName :: BS.ByteString
}
  deriving (Show, Eq)
