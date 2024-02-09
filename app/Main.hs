module Main where


import           Cardano.Api
  ( InAnyShelleyBasedEra (..)
  )
import           CCApi.Utils
  ( hexToByteString
  , readByteStringTx
  , getTxBodyAndWitnesses
  , getTxIns
  , getTxInsCollateral
  )
import           Data.Word (Word8)
import           Text.Pretty.Simple (pPrintOpt, CheckColorTty (..), OutputOptions (..), defaultOutputOptionsDarkBg)
import           System.Environment (getArgs)


main :: IO ()
main = do
  allArgs <- getArgs
  case allArgs of
    []         -> putStrLn "Please enter a tx CBOR hex."
    hexStr : _ ->
      case hexToByteString hexStr of
        Right bs -> do
          case readByteStringTx bs of
            Just (InAnyShelleyBasedEra _ tx) -> do
              let (body, _) = getTxBodyAndWitnesses tx
                  txIns = getTxIns body
                  cols = getTxInsCollateral body
              putStrLn ""
              printInColor green "==========================================================================================="
              pPrint txIns
              printInColor green "==========================================================================================="
              pPrint cols
              printInColor green "==========================================================================================="
            Nothing ->
              printInColor red "Invalid transaction."
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
