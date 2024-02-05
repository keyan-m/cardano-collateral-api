module CCApi.Utils
  ( hexToByteString
  ) where


import           Data.Base16.Types (assertBase16)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import           Data.String (fromString)


hexToByteString :: String -> Either String ByteString
hexToByteString str =
  let
    bs = fromString str :: ByteString
  in
  if B16.isBase16 bs
  then Right $ B16.decodeBase16 $ assertBase16 bs
  else Left "Invalid hex"

