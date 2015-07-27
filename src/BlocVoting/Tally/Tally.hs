module BlocVoting.Tally.Tally where

import BlocVoting.Tally.Resolution
import BlocVoting.Tally.Vote

data Tally = Tally {
    tResolution :: Resolution
  , tVotes :: [Vote]
}
  deriving (Show, Eq)
