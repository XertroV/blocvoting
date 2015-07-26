module BlocVoting.Tally.Voter where

import qualified Data.ByteString as BS

import BlocVoting.Tally.Vote

type Voter = BS.ByteString

supersedeIfFrom :: Voter -> Vote -> Vote
supersedeIfFrom voter vote@(Vote cScalar cSender height superseded) | voter == cSender = Vote cScalar cSender height True
                                                                    | otherwise = vote
