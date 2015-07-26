import qualified Data.ByteString as BS

data NetworkSettings = NetworkSettings {
    adminAddress :: BS.ByteString
  , networkName :: BS.ByteString
}
  deriving (Show, Eq)
