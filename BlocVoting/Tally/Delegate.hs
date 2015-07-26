module BlocVoting.Tally.Delegate where

import BlocVoting.Tally.Voter

data Delegate = Delegate {
    delegater :: Voter
  , delegate :: Voter
}
  deriving (Show, Eq)
