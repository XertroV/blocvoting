module BlocVoting.Bitcoin.Base58 where

import qualified Data.ByteString.Base58 as B58

import qualified Network.Haskoin.Crypto as HCrypto (decodeBase58Check)

encodeBase58 = B58.encodeBase58 B58.bitcoinAlphabet

decodeBase58Check = HCrypto.decodeBase58Check
