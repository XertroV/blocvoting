module BlocVoting.Tally.Vote where

import BlocVoting.Tally.Voter

data Vote = Vote {
    voteScalar :: Int
  , voter :: Voter
  , height :: Integer
  , superseded :: Bool
}
  deriving (Show, Eq)
