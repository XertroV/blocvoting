module BlocVoting.Tally where

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Maybe (fromJust)

import BlocVoting.Instructions
import BlocVoting.Nulldata
import qualified BlocVoting.Instructions.Create as Create
import qualified BlocVoting.Instructions.Empower as Empower
import qualified BlocVoting.Instructions.ModRes as ModRes
import qualified BlocVoting.Instructions.Cast as Cast


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

data Tally = Tally {
    tResolution :: Resolution 
  , tVotes :: [Vote]
}
  deriving (Show, Eq)

data Resolution = Resolution {
    rCategories :: Int
  , rEndTimestamp :: Int
  , rName :: BS.ByteString
  , rUrl :: BS.ByteString
  , rVotesFor :: Integer
  , rVotesTotal :: Integer
  , rResolved :: Bool
}
  deriving (Show, Eq)

type Voter = BS.ByteString

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
    voteScalar :: Int
  , voter :: Voter
  , height :: Integer
  , superseded :: Bool
}
  deriving (Show, Eq)
  
  
  
getEmpowerment :: GrandTally -> Voter -> Int
getEmpowerment gt v = fromJust $ Map.lookup v (gtVoters gt)


isVoter :: GrandTally -> Voter -> Int
isVoter gt v = Map.member v (gtVoters gt)



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


modGTModRes :: GrandTally -> Maybe ModRes.OpModRes -> GrandTally
modGTModRes gt (Just (ModRes.OpModRes cats endTimestamp resolution url nd)) = GrandTally {
    gtNetworkSettings = gtNetworkSettings gt
  , gtTallies = Map.insert resolution newTally (gtTallies gt)
  , gtVoters = gtVoters gt
  , gtDelegations = gtDelegations gt
  , gtTransfers = gtTransfers gt
} where newTally | isMember = Tally (Resolution cats endTimestamp resolution url (rVotesFor origRes) (rVotesTotal origRes) (rResolved origRes)) (tVotes origTally)
                 | otherwise = Tally (Resolution cats endTimestamp resolution url 0 0 False) []
        isMember = Map.member resolution (gtTallies gt)
        origTally = fromJust $ Map.lookup resolution (gtTallies gt)
        origRes = tResolution origTally
modGTModRes gt _ = gt

supersedeIfFrom :: Voter -> Vote -> Vote
supersedeIfFrom voter vote@(Vote cScalar cSender height superseded) | voter == cSender = Vote cScalar cSender height True
                                                                    | otherwise = vote


modGTCast :: GrandTally -> Maybe Cast.OpCast -> GrandTally
modGTCast gt (Just (Cast.OpCast cScalar cRes cSender nd)) = GrandTally {
    gtNetworkSettings = gtNetworkSettings gt
  , gtTallies = newTallies
  , gtVoters = gtVoters gt
  , gtDelegations = gtDelegations gt
  , gtTransfers = gtTransfers gt
} where newTallies = | isMember = Map.insert cRes $ Tally () (Vote cScalar cSender 0 False):(map (supersedeIfFrom cSender) relVotes)
                     | otherwise = gtTallies gt
        isMember = Map.member cRes (gtTallies gt)
        relTally@(Tally relRes relVotes) = fromJust $ Map.lookup cRes (gtTallies gt)
modGTCast gt _ = gt


applyInstruction gt nd@(Nulldata msg sender)
            | opcode == op_EMPOWER = modGTEmpower gt (Empower.fromNulldata nd)
            | opcode == op_MOD_RES = modGTModRes gt (ModRes.fromNulldata nd)
            | opcode == op_CAST = modGTCast gt (Cast.fromNulldata nd)
            | otherwise = gt
            where opcode = BS.head msg
applyInstruction gt _ = gt


listOfInstructionsToGrandTally :: [Nulldata] -> GrandTally
listOfInstructionsToGrandTally instructions = foldl applyInstruction initNetwork remainingInstructions
  where initNetwork = createGT $ fromJust $ Create.fromNulldata creationInstruction
        (creationInstruction:remainingInstructions) = dropWhile (not . operationIs op_CREATE) instructions
