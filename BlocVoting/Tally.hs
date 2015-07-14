module BlocVoting.Tally where

import qualified Data.ByteString as BS
import qualified Data.Map as Map

import BlocVoting.Instructions
import BlocVoting.Nulldata
import qualified BlocVoting.Instructions.Create as Create
import qualified BlocVoting.Instructions.Empower as Empower


type Address = BS.ByteString

data GrandTally = GrandTally {
    gtNetworkSettings :: NetworkSettings
  , gtTallies :: Map.Map BS.ByteString Tally
  , gtVoters :: Map.Map BS.ByteString Int
  , gtDelegations :: [Delegate]
  , gtTransfers :: [Transfer]
}
  deriving (Show, Eq)

data NetworkSettings = NetworkSettings {
    adminAddress :: BS.ByteString
  , networkName :: BS.ByteString
}
  deriving (Show, Eq)

data Tally = Tally Resolution [Vote]
  deriving (Show, Eq)

data Resolution = Resolution {
    resName :: BS.ByteString
  , categories :: Integer
  , url :: BS.ByteString
  , endTimestamp :: Integer
  , votesFor :: Integer
  , votesTotal :: Integer
  , resolved :: Bool
}
  deriving (Show, Eq)

data Voter = Voter {
    voterAddress :: BS.ByteString
  , votesEmpowered :: Integer
}
  deriving (Show, Eq)

data Transfer = Transfer {
    fromVoter :: Voter
  , afterTime :: Integer
  , newAddress :: BS.ByteString
}
  deriving (Show, Eq)

data Delegate = Delegate {
    delegater :: Voter
  , delegate :: Voter
}
  deriving (Show, Eq)

data Vote = Vote {
    voteNum :: Integer
  , voter :: Voter
  , height :: Integer
  , superseded :: Bool
}
  deriving (Show, Eq)



createGT :: Create.OpCreate -> GrandTally
createGT (Create.OpCreate networkName adminAddress) = GrandTally {
    gtNetworkSettings = NetworkSettings adminAddress networkName
  , gtTallies = Map.fromList []
  , gtVoters = Map.insert adminAddress 1 Map.empty
  , gtDelegations = []
  , gtTransfers = []
}


modGTEmpower :: GrandTally -> Empower.OpEmpower -> GrandTally
modGTEmpower gt (Empower.OpEmpower votes address) = GrandTally {
    gtNetworkSettings = gtNetworkSettings gt
  , gtTallies = gtTallies gt
  , gtVoters = Map.insert address votes (gtVoters gt)
  , gtDelegations = gtDelegations gt
  , gtTransfers = gtTransfers gt
}






applyInstruction gt nd@(Nulldata msg sender)
            | opcode == op_EMPOWER = modGTEmpower gt (Empower.fromNulldata nd)
            | otherwise = gt
            where opcode = BS.head msg
applyInstruction gt _ = gt


listOfInstructionsToGrandTally :: [Nulldata] -> GrandTally
listOfInstructionsToGrandTally instructions = foldl applyInstruction initNetwork remainingInstructions
  where initNetwork = createGT $ Create.fromNulldata creationInstruction
        (creationInstruction:remainingInstructions) = dropWhile (not . operationIs op_CREATE) instructions
