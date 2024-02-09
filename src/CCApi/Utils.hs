{-# LANGUAGE RankNTypes #-}


module CCApi.Utils
  ( hexToByteString
  , readByteStringTx
  , getTxBodyAndWitnesses
  , getTxIns
  , getTxInsCollateral
  , scriptIsClaimedValid
  ) where


import qualified Cardano.Api as Api
import           Cardano.Api
  ( FromSomeType (..)
  , SerialiseAsCBOR
  , Tx
  , TxBody
  , KeyWitness
  , InAnyShelleyBasedEra (..)
  , AsType (..)
  , ShelleyBasedEra (..)
  , deserialiseFromCBOR
  , getTxBody
  , getTxWitnesses
  )
import           Data.Base16.Types (assertBase16)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Base16 as B16
import           Data.String (fromString)
import           Data.Typeable (Typeable)


hexToByteString :: String -> Either String ByteString
hexToByteString str =
  let
    bs = fromString str :: ByteString
  in
  if B16.isBase16 bs
  then Right $ B16.decodeBase16 $ assertBase16 bs
  else Left "Invalid hex"


deserialiseOne :: forall b . ()
  => FromSomeType SerialiseAsCBOR b
  -> ByteString
  -> Maybe b
deserialiseOne (FromSomeType ttoken f) bs =
  case f <$> deserialiseFromCBOR ttoken bs of
    Left _  -> Nothing
    Right x -> Just x


deserialiseAnyOf :: forall b . ()
  => [FromSomeType SerialiseAsCBOR b]
  -> ByteString
  -> Maybe b
deserialiseAnyOf ts te =
  let
    go []        = Nothing
    go (t : ts') =
      case deserialiseOne t te of
        r@(Just _) -> r
        _          -> go ts'
  in
  go ts


readByteStringTx :: ByteString -> Maybe (InAnyShelleyBasedEra Tx)
readByteStringTx =
  deserialiseAnyOf
    [ FromSomeType (AsTx AsShelleyEra) (InAnyShelleyBasedEra ShelleyBasedEraShelley)
    , FromSomeType (AsTx AsAllegraEra) (InAnyShelleyBasedEra ShelleyBasedEraAllegra)
    , FromSomeType (AsTx AsMaryEra)    (InAnyShelleyBasedEra ShelleyBasedEraMary)
    , FromSomeType (AsTx AsAlonzoEra)  (InAnyShelleyBasedEra ShelleyBasedEraAlonzo)
    , FromSomeType (AsTx AsBabbageEra) (InAnyShelleyBasedEra ShelleyBasedEraBabbage)
    , FromSomeType (AsTx AsConwayEra)  (InAnyShelleyBasedEra ShelleyBasedEraConway)
    ]

getTxBodyAndWitnesses :: Tx era -> (TxBody era, [KeyWitness era])
getTxBodyAndWitnesses tx = (getTxBody tx, getTxWitnesses tx)


getTxIns :: TxBody era -> [Api.TxIn]
getTxIns = fmap fst . Api.txIns . Api.getTxBodyContent

getTxInsCollateral :: TxBody era -> [Api.TxIn]
getTxInsCollateral body =
  case Api.txInsCollateral (Api.getTxBodyContent body) of
    Api.TxInsCollateral _ ins -> ins
    _                         -> []

scriptIsClaimedValid :: TxBody era -> Bool
scriptIsClaimedValid body =
  case Api.txScriptValidity (Api.getTxBodyContent body) of
    Api.TxScriptValidity _ Api.ScriptValid -> True
    _                                    -> False
