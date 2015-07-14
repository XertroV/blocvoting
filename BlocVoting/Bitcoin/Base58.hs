module BlocVoting.Bitcoin.Base58 where

import qualified Data.ByteString.Base58 as B58

encodeBase58 = B58.encodeBase58 B58.bitcoinAlphabet
