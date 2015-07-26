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
  , gtDelegations :: [Delegate]
  , gtTransfers :: [Transfer]
}
  deriving (Show, Eq)


createGT :: Create.OpCreate -> GrandTally
createGT (Create.OpCreate networkName adminAddress nd) = GrandTally {
    gtNetworkSettings = NetworkSettings adminAddress networkName
  , gtTallies = Map.empty
  , gtVoters = Map.empty
  , gtDelegations = []
  , gtTransfers = []
}



modGTEmpower :: GrandTally -> Maybe Empower.OpEmpower -> GrandTally
modGTEmpower gt (Just (Empower.OpEmpower votes address nd)) = GrandTally {
    gtNetworkSettings = gtNetworkSettings gt
  , gtTallies = gtTallies gt
  , gtVoters = Map.insert address votes (gtVoters gt)
  , gtDelegations = gtDelegations gt
  , gtTransfers = gtTransfers gt
}
modGTEmpower gt _ = gt
