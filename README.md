# BlocVoting

Deleagtive Democracy on the Blockchain.

## Overview

BlocVoting stores a record of all votes, ballots, delegations, etc in special metadata 
transactions that are broadcast on the Bitcoin network.

Anyone can then download the relevant software and scan the blockchain to validate the outcome of 
a vote.

You can find more complete python versions of the protocol in the following repositories:

* [nvbclient](https://github.com/XertroV/nvbclient)
* [nvblib](https://github.com/XertroV/nvblib)
* [nvbtally](https://github.com/XertroV/nvbtally)

## Downloads

Binaries will become available with the first stable version.

## Building

You will need [stack](https://github.com/commercialhaskell/stack) installed.

In the source directory run `stack build --install-ghc` to build the binaries.
