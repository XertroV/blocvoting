module BlocVoting.Tally.GrandTally where

import qualified Data.ByteString as BS
import qualified Data.Map as Map

import BlocVoting.Tally.NetworkSettings
import BlocVoting.Tally.Tally
import BlocVoting.Tally.Delegate
import BlocVoting.Tally.Transfer


data GrandTally = GrandTally {
    gtNetworkSettings :: NetworkSettings
  , gtTallies :: Map.Map BS.ByteString Tally
  , gtVoters :: Map.Map BS.ByteString Int
  , gtDelegations :: Map.Map BS.ByteString BS.ByteString
  , gtTransfers :: Map.Map Address (Int, Address)
}
  deriving (Show, Eq)



modGTVoters :: GrandTally -> Map.Map BS.ByteString Int -> GrandTally
modGTVoters gt newVoters = GrandTally {
    gtNetworkSettings = gtNetworkSettings gt
  , gtTallies = gtTallies gt
  , gtVoters = newVoters
  , gtDelegations = gtDelegations gt
  , gtTransfers = gtTransfers gt
}

modGTTallies :: GrandTally -> Map.Map BS.ByteString Tally -> GrandTally
modGTTallies gt newTallies = GrandTally {
    gtNetworkSettings = gtNetworkSettings gt
  , gtTallies = newTallies
  , gtVoters = gtVoters gt
  , gtDelegations = gtDelegations gt
  , gtTransfers = gtTransfers gt
}

modGTDelegate :: GrandTally -> Map.Map BS.ByteString BS.ByteString -> GrandTally
modGTDelegate gt newDelegates = GrandTally {
    gtNetworkSettings = gtNetworkSettings gt
  , gtTallies = gtTallies gt
  , gtVoters = gtVoters gt
  , gtDelegations = newDelegates
  , gtTransfers = gtTransfers gt
}
