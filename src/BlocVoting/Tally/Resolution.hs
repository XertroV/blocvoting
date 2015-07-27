module BlocVoting.Tally.Resolution where

import qualified Data.ByteString as BS

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


updateResolution :: Resolution -> Integer -> Integer -> Resolution
updateResolution (Resolution cats endT name url for total resolved) newForVotes newTotalVotes =
    Resolution cats endT name url (for +  newForVotes) (total + newTotalVotes) resolved
