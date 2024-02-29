{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Blockfrost.Client as BF
import           Data.Base16.Types (assertBase16)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Lazy as LBS
import           Data.String (fromString)
import           System.Environment (getArgs)

hexToByteString :: String -> Either String ByteString
hexToByteString str =
  let
    bs :: ByteString
    bs =
      case str of
        '0' : 'x' : rest -> fromString rest
        _                -> fromString str
  in
  if B16.isBase16 bs
  then Right $ B16.decodeBase16 $ assertBase16 bs
  else Left "Invalid hex"

txString :: String
txString = "84a90083825820c71d17bfa2003f9d65e294505a70743bc5f676b80d10d2f78a6ec685284f585400825820dd1a33a0749853711b08b26cbd58359de61966c9789fe066d0829e0d62d4051702825820dd1a33a0749853711b08b26cbd58359de61966c9789fe066d0829e0d62d40517030184a300581d70fb838cd46053f398959f735f23e427530478ade701f8a84b5bed111f01821a002dc6c0a3581c26ca4cccf6dd50c681248e9e6e190fe2d7e3f88b570485d34f894f1ba3411701411801411901581c30324ac391ee68b47478e3c64387379160d169265b47d2ec88b76385a3410c01410d01411201581cb43801248dde11e46a72fe6d6f2d0cada039615d8bdfc23c45805b43a150546573746e65744578616d706c65333401028201d8185830d8799f1a2408a9bf581cf9f653a20bf208aa7973625be31abacd517cf82e676f090540fdaac31b0000018f85bab588ff82583900f9f653a20bf208aa7973625be31abacd517cf82e676f090540fdaac36695446174c5ea77d6a151ae780471737893dee12e8dc579fd7c519a1a1707ff00a300581d7064ce2d14a5cd5f93db6d7f8a6b1af01b05694a1195592ebba485915b011a319442c0028201d8185830d8799f581c30324ac391ee68b47478e3c64387379160d169265b47d2ec88b763851a07bc97401b0000018d3ac617ceff82583900f9f653a20bf208aa7973625be31abacd517cf82e676f090540fdaac36695446174c5ea77d6a151ae780471737893dee12e8dc579fd7c519a821b0000000334353638a3581c26ca4cccf6dd50c681248e9e6e190fe2d7e3f88b570485d34f894f1ba6410101410201410301411301411501411601581cb43801248dde11e46a72fe6d6f2d0cada039615d8bdfc23c45805b43a550546573746e65744578616d706c6533380150546573746e65744578616d706c6534360150546573746e65744578616d706c6534390150546573746e65744578616d706c6535300150546573746e65744578616d706c65353101581cd87c6634fe9440a4ec0273057d76725c9e484838574b12d20c2cff4aa4410a01410b01410c01410d01021a0003372d031a0257045b081a0256fd530b5820d41a1d628d0e760bcab99ffc150807b31e07007d620e2dbff4ee6f7dca8dc2420d8182582081564151a58d857553322e936406d6e020ec1b376199ac71b1f9098176babed9050e81581cf9f653a20bf208aa7973625be31abacd517cf82e676f090540fdaac312828258203b2b67bdb9c81fa116936c21903b4c59431945280352a5f7c7858beea1efd55601825820dd1a33a0749853711b08b26cbd58359de61966c9789fe066d0829e0d62d4051702a203800581840001d8799fd8799f581cf9f653a20bf208aa7973625be31abacd517cf82e676f090540fdaac31a07bc97401b000000025561b000a3410c01410d01411201ffff820000f5f6"

mTx :: Either String BF.CBORString
mTx = do
  bs <- hexToByteString txString
  return $ BF.CBORString $ LBS.fromStrict bs

main :: IO ()
main =
  let
    fromMethod = \case
      "base16"     ->
        case mTx of
          Right cbor -> cbor
          Left  _    -> undefined
      "fromString" ->
        BF.CBORString $ fromString txString
      _            ->
        undefined
  in do
  allArgs <- getArgs
  case allArgs of
    "submit" : method : _ -> do
      submitRes <- BF.submitTx $ fromMethod method
      print submitRes
    "eval"   : method : _ -> do
      evalRes <- BF.txEvaluate $ fromMethod method
      print evalRes
    _                -> do
      putStrLn "usage: <ACTION> <HEX_DECODE_METHOD>"
      putStrLn "\t<ACTION>\t\t(submit | eval)"
      putStrLn "\t<HEX_DECODE_METHOD>\t(base16 | fromString)"
