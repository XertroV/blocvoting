module BlocVoting.Tally where

import qualified Data.ByteString as BS
import qualified Data.Map as Map
import Data.Maybe (fromJust)

import BlocVoting.Instructions
import BlocVoting.Nulldata
import BlocVoting.Bitcoin.Base58
import qualified BlocVoting.Instructions.Create as Create
import qualified BlocVoting.Instructions.Empower as Empower
import qualified BlocVoting.Instructions.ModRes as ModRes
import qualified BlocVoting.Instructions.Cast as Cast
import qualified BlocVoting.Instructions.Delegate as Dlg



getEmpowerment :: GrandTally -> Voter -> Int
getEmpowerment gt v = case Map.lookup v (gtVoters gt) of Just i -> i
                                                         Nothing -> 0

isVoter :: GrandTally -> Voter -> Bool
isVoter gt v = Map.member v (gtVoters gt)
--
--
-- data NetworkSettings = NetworkSettings {
--     adminAddress :: BS.ByteString
--   , networkName :: BS.ByteString
-- }
--   deriving (Show, Eq)
--
-- data Tally = Tally {
--     tResolution :: Resolution
--   , tVotes :: [Vote]
-- }
--   deriving (Show, Eq)
--
-- data Resolution = Resolution {
--     rCategories :: Int
--   , rEndTimestamp :: Int
--   , rName :: BS.ByteString
--   , rUrl :: BS.ByteString
--   , rVotesFor :: Integer
--   , rVotesTotal :: Integer
--   , rResolved :: Bool
-- }
--   deriving (Show, Eq)
--
-- type Voter = BS.ByteString
--
-- data Transfer = Transfer {
--     fromVoter :: Voter
--   , afterTime :: Integer
--   , newAddress :: BS.ByteString
-- }
--   deriving (Show, Eq)
--
-- data Delegate = Delegate {
--     delegater :: Voter
--   , delegate :: Voter
-- }
--   deriving (Show, Eq)
--
-- data Vote = Vote {
--     voteScalar :: Int
--   , voter :: Voter
--   , height :: Integer
--   , superseded :: Bool
-- }
--   deriving (Show, Eq)
--


getEmpowerment :: GrandTally -> Voter -> Int
getEmpowerment gt v = case Map.lookup v (gtVoters gt) of Just i -> i
                                                       Nothing -> 0

isVoter :: GrandTally -> Voter -> Bool
isVoter gt v = Map.member v (gtVoters gt)


supersedeIfFrom :: Voter -> Vote -> Vote
supersedeIfFrom thisVoter vote@(Vote cScalar cSender h _) | thisVoter == cSender = Vote cScalar cSender h True
                                                          | otherwise            = vote


isAdminOf :: GrandTally -> BS.ByteString -> Bool
isAdminOf gt userAddr = userAddr == nsAdminAddress (gtNetworkSettings gt)


applyOpEmpower :: GrandTally -> Maybe Empower.OpEmpower -> GrandTally
applyOpEmpower gt (Just (Empower.OpEmpower votes address nd))
    | isAdminOf gt . ndAddress $ nd = gt2
    | otherwise = gt
    where gt1 = modGTVoters gt (Map.insert address votes (gtVoters gt))
          gt2 = modGTDelegate gt1 (Map.insert address address (gtDelegations gt))
applyOpEmpower gt _ = gt


applyOpModRes :: GrandTally -> Maybe ModRes.OpModRes -> GrandTally
applyOpModRes gt (Just (ModRes.OpModRes cats endTimestamp resolution url nd))
  | isAdminOf gt (ndAddress nd) = modGTTallies gt newTallies
  | otherwise = gt
  where newTallies = Map.insert resolution newTally (gtTallies gt)
        newTally | isMember = Tally (Resolution cats endTimestamp resolution url (rVotesFor origRes) (rVotesTotal origRes) (rResolved origRes)) (tVotes origTally)
                 | otherwise = Tally (Resolution cats endTimestamp resolution url 0 0 False) []
        isMember = Map.member resolution (gtTallies gt)
        origTally = fromJust $ Map.lookup resolution (gtTallies gt)
        origRes = tResolution origTally
applyOpModRes gt _ = gt

applyOpCast :: GrandTally -> Maybe Cast.OpCast -> GrandTally
applyOpCast gt (Just (Cast.OpCast cScalar cRes nd@(Nulldata _ cSender))) = gt1
  where gt1 = modGTTallies gt newTallies
        newTallies | isMember = Map.insert cRes (Tally newRes $ (Vote cScalar cSender 0 False):(map (supersedeIfFrom cSender) theVotes)) (gtTallies gt)
                   | otherwise = gtTallies gt
        isMember = Map.member cRes (gtTallies gt)
        (Just theTally@(Tally theRes theVotes)) = Map.lookup cRes (gtTallies gt)
        newRes = updateResolution theRes newForVotes newTotalVotes
        newForVotes = toInteger $ cScalar * empowerment  -- each voter really has 255 votes
        newTotalVotes = toInteger $ 255 * empowerment
        empowerment = getEmpowerment gt cSender
applyOpCast gt _ = gt

applyOpDelegate :: GrandTally -> Maybe Dlg.OpDelegate -> GrandTally
applyOpDelegate gt (Just (Dlg.OpDelegate dCats dAddr nd)) = modGTDelegate gt newDelegates
  where newDelegates = Map.insert (ndAddress nd) (encodeBase58 dAddr) (gtDelegations gt)
applyOpDelegate gt _ = gt


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



applyInstruction :: GrandTally -> Nulldata -> GrandTally
applyInstruction gt nd@(Nulldata msg sender)
            | opcode == op_EMPOWER = applyOpEmpower gt (Empower.fromNulldata nd)
            | opcode == op_MOD_RES = applyOpModRes gt (ModRes.fromNulldata nd)
            | opcode == op_CAST = applyOpCast gt (Cast.fromNulldata nd)
            | opcode == op_DELEGATE = applyOpDelegate gt (Dlg.fromNulldata nd)
            | otherwise = gt
            where opcode = BS.head msg
applyInstruction gt _ = gt


listOfInstructionsToGrandTally :: [Nulldata] -> GrandTally
listOfInstructionsToGrandTally instructions = foldl applyInstruction initNetwork remainingInstructions
  where initNetwork = createGT $ fromJust $ Create.fromNulldata creationInstruction
        (creationInstruction:remainingInstructions) = dropWhile (not . operationIs op_CREATE) instructions
