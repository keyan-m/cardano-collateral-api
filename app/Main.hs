{-# LANGUAGE OverloadedStrings #-}


module Main where


import qualified Blockfrost.Client as BF
import qualified Blockfrost.Lens as BFL
import           Cardano.Api
  ( InAnyShelleyBasedEra (..)
  )
import           CCApi.Utils
  ( hexToByteString
  , readByteStringTx
  , getTxBodyAndWitnesses
  , getTxIns
  , getTxInsCollateral
  , scriptIsClaimedValid
  )
import           Control.Lens ((^.))
-- import qualified Data.ByteString.Lazy as LBS
import           Data.String (fromString)
import           Data.Word (Word8)
import           Text.Pretty.Simple (pPrintOpt, CheckColorTty (..), OutputOptions (..), defaultOutputOptionsDarkBg)
import           System.Environment (getArgs)

walletAddress :: BF.Address
walletAddress = BF.Address "addr_test1qp83nuj43rvtmme8f3n4sprs93scukz5myrxnnmpmmhmu7jm5afn3re7sse8zseg6pm0nn00dv99j97dh9pc2jtmtx5q2mh54q"

utxoTxHash :: BF.TxHash
utxoTxHash = BF.TxHash "8685745144edfd533045caf401dd5b15b9f6ae966077a101d7f60311350ae573"

utxoOutputIndex :: Integer
utxoOutputIndex = 0

main :: IO ()
main = do
  allArgs <- getArgs
  case allArgs of
    []         -> putStrLn "Please enter a tx CBOR hex"
    hexStr : _ ->
      case hexToByteString hexStr of
        Right bs -> do
          case readByteStringTx bs of
            Just (InAnyShelleyBasedEra _ tx) ->
              let
                (body, _) = getTxBodyAndWitnesses tx
              in
              if scriptIsClaimedValid body
              then do
                let txIns = getTxIns body
                    cols = getTxInsCollateral body
                bf <- BF.projectFromFile ".blockfrost"
                utxosRes <- BF.runBlockfrost bf $ BF.getAddressUtxos walletAddress
                let resColUTxO =
                      case utxosRes of
                        Right initUTxOs ->
                          case filter ((== walletAddress) . (^. BFL.address)) initUTxOs of
                            [u] ->
                              let
                                cond =
                                     u ^. BFL.txHash      == utxoTxHash
                                  && u ^. BFL.outputIndex == utxoOutputIndex
                              in
                              if cond
                              then Right u
                              else Left "Did not find the collateral UTxO in wallet"
                            _   ->
                              Left "Exactly one UTxO was expected to be present in the wallet"
                        Left err       ->
                          Left $ show err
                    -- cborStr = BF.CBORString $ LBS.fromStrict bs
                    cborStr = BF.CBORString $ fromString hexStr
                printInColor yellow $ show resColUTxO
                evalRes <- BF.runBlockfrost bf $ BF.txEvaluate cborStr
                case evalRes of
                  Left err -> do
                    printInColor red "Blockfrost request failed"
                    pPrint err
                  Right _ -> do
                    putStrLn ""
                    printInColor green "Transaction executed successfully"
                    putStrLn ""
                    printInColor green "==========================================================================================="
                    pPrint txIns
                    printInColor green "==========================================================================================="
                    pPrint cols
                    printInColor green "==========================================================================================="
              else printInColor red "Transaction's script validity is false"
            Nothing ->
              printInColor red "Invalid transaction"
        Left err -> do
          printInColor red err


pPrint :: Show a => a -> IO ()
pPrint = pPrintOpt CheckColorTty (defaultOutputOptionsDarkBg {outputOptionsIndentAmount = 1})

printInColor :: String -> String -> IO ()
printInColor color msg = do
  putStr color
  putStrLn msg
  putStr noColor


mkRGBColor :: Word8 -> Word8 -> Word8 -> String
mkRGBColor r g b =
  "\ESC[38;2;" ++ show r ++ ";" ++ show g ++ ";" ++ show b ++ "m"
yellow  :: String
yellow  = mkRGBColor 252 209 47
red     :: String
red     = mkRGBColor 239 76  40
green   :: String
green   = mkRGBColor 25  176 92
purple  :: String
purple  = mkRGBColor 155 39  255
noColor :: String

lightGreen :: String
lightGreen = mkRGBColor 125 200 125

noColor = "\ESC[0m"
