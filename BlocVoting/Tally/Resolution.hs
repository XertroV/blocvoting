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
