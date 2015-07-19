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
import qualified BlocVoting.Instructions.Delegate as Dlg


type Address = BS.ByteString

data GrandTally = GrandTally {
    gtNetworkSettings :: NetworkSettings
  , gtTallies :: Map.Map BS.ByteString Tally
  , gtVoters :: Map.Map BS.ByteString Int
  , gtDelegations :: Map.Map BS.ByteString BS.ByteString  -- this will need to change when categories are implemented
  , gtTransfers :: [Transfer]
}
  deriving (Show, Eq)

data NetworkSettings = NetworkSettings {
    nsAdminAddress :: BS.ByteString
  , nsNetworkName :: BS.ByteString
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

data Vote = Vote {
    voteScalar :: Int
  , voter :: Voter
  , height :: Integer
  , superseded :: Bool
}
  deriving (Show, Eq)



getEmpowerment :: GrandTally -> Voter -> Int
getEmpowerment gt v = case Map.lookup v (gtVoters gt) of Just i -> i
                                                         Nothing -> 0

isVoter :: GrandTally -> Voter -> Bool
isVoter gt v = Map.member v (gtVoters gt)

supersedeIfFrom :: Voter -> Vote -> Vote
supersedeIfFrom thisVoter vote@(Vote cScalar cSender h _) | thisVoter == cSender = Vote cScalar cSender h True
                                                          | otherwise            = vote

updateResolution :: Resolution -> Integer -> Integer -> Resolution
updateResolution (Resolution cats endT name url for total resolved) newForVotes newTotalVotes =
    Resolution cats endT name url (for +  newForVotes) (total + newTotalVotes) resolved



createGT :: Create.OpCreate -> GrandTally
createGT (Create.OpCreate cNetName cAdminAddr _) = GrandTally {
    gtNetworkSettings = NetworkSettings cAdminAddr cNetName
  , gtTallies = Map.empty
  , gtVoters = Map.empty
  , gtDelegations = Map.empty
  , gtTransfers = []
}


isAdminOf :: GrandTally -> BS.ByteString -> Bool
isAdminOf gt userAddr = userAddr == (nsAdminAddress $ gtNetworkSettings gt)


modGTEmpower :: GrandTally -> Maybe Empower.OpEmpower -> GrandTally
modGTEmpower gt (Just (Empower.OpEmpower votes address nd))
    | isAdminOf gt . ndAddress $ nd = GrandTally {
                gtNetworkSettings = gtNetworkSettings gt
              , gtTallies = gtTallies gt
              , gtVoters = Map.insert address votes (gtVoters gt)
              , gtDelegations = gtDelegations gt
              , gtTransfers = gtTransfers gt
            }
    | otherwise = gt
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


modGTCast :: GrandTally -> Maybe Cast.OpCast -> GrandTally
modGTCast gt (Just (Cast.OpCast cScalar cRes nd@(Nulldata _ cSender))) = GrandTally {
    gtNetworkSettings = gtNetworkSettings gt
  , gtTallies = newTallies
  , gtVoters = gtVoters gt
  , gtDelegations = gtDelegations gt
  , gtTransfers = gtTransfers gt
} where isMember = Map.member cRes (gtTallies gt)
        newTallies | isMember = Map.insert cRes (Tally newRes $ (Vote cScalar cSender 0 False):(map (supersedeIfFrom cSender) relVotes)) (gtTallies gt)
                   | otherwise = gtTallies gt
        (Just relTally@(Tally relRes relVotes)) = Map.lookup cRes (gtTallies gt)
        newRes = updateResolution relRes newForVotes newTotalVotes
        newForVotes = toInteger $ cScalar * empowerment  -- each voter really has 255 votes
        newTotalVotes = toInteger $ 255 * empowerment
        empowerment = getEmpowerment gt cSender
modGTCast gt _ = gt


modGTDelegate :: GrandTally -> Maybe Dlg.OpDelegate -> GrandTally
modGTDelegate gt (Just (Dlg.OpDelegate dCats dAddr nd)) = GrandTally {
    gtNetworkSettings = gtNetworkSettings gt
  , gtTallies = gtTallies gt
  , gtVoters = gtVoters gt
  , gtDelegations = newDelegates
  , gtTransfers = gtTransfers gt
} where newDelegates = Map.insert (ndAddress nd) dAddr (gtDelegations gt)
modGTDelegate gt _ = gt




applyInstruction :: GrandTally -> Nulldata -> GrandTally
applyInstruction gt nd@(Nulldata msg sender)
            | opcode == op_EMPOWER = modGTEmpower gt (Empower.fromNulldata nd)
            | opcode == op_MOD_RES = modGTModRes gt (ModRes.fromNulldata nd)
            | opcode == op_CAST = modGTCast gt (Cast.fromNulldata nd)
            | otherwise = gt
            where opcode = BS.head msg


listOfInstructionsToGrandTally :: [Nulldata] -> GrandTally
listOfInstructionsToGrandTally instructions = foldl applyInstruction initNetwork remainingInstructions
  where initNetwork = createGT $ fromJust $ Create.fromNulldata creationInstruction
        (creationInstruction:remainingInstructions) = dropWhile (not . operationIs op_CREATE) instructions
