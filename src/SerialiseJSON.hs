{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module SerialiseJSON (testBuy, testUpdate, testCancel, testDatum, testAddress, dumpAddress) where

-- import Cardano.Binary
--   (    serializeEncoding
--   )

-- import Cardano.Ledger.Alonzo.Language (Language)
-- import Cardano.Ledger.Alonzo.PParams

-- import Data.Set (Set)
-- import qualified Data.Set as Set
-- import qualified Data.ByteString.Base16 as B16
-- import Data.ByteString.Lazy as LBS

import Data.Aeson as Json ( encode )
import Data.ByteString.Lazy qualified as LB

import Ledger
    ( scriptAddress, scriptHashAddress, Address )

import           Paradiso.Contracts.FixedPrice.Utility             (testnetSettings)
import Paradiso.Contracts.FixedPrice.OnChain  as O2 (fixedPriceValidator, fixedPriceValidatorHash)

import Cardano.Api
    ( scriptDataToJson,
      ScriptDataJsonSchema(ScriptDataJsonDetailedSchema) )
import Cardano.Api.Shelley ( fromPlutusData )
import qualified PlutusTx

import Paradiso.Contracts.FixedPrice.Types (FixedPriceAction(..), FixedPriceSale(..))

dumpAddress :: Ledger.Address
dumpAddress = scriptAddress $ fixedPriceValidator testnetSettings

testAddress :: IO ()
testAddress = do 
  putStrLn $ show (scriptHashAddress $ fixedPriceValidatorHash testnetSettings)
  putStrLn "Done"

testBuy :: IO ()
testBuy = do 
  writeData "buy.json" Buy
  putStrLn "Done"

testUpdate :: IO ()
testUpdate = do 
  writeData "update.json" Update
  putStrLn "Done"

testCancel :: IO ()
testCancel = do 
  writeData "cancel.json" Cancel
  putStrLn "Done"

saleEx :: FixedPriceSale
saleEx = FixedPriceSale
    { sellerPubKeyHash    = "af"
    , assetCurrency   = "66"
    , assetName    = "Vendere"
    , price = 1
    , royaltyPubKeyHash  = "af"
    , royaltyPercentage  = 0
    }

testDatum :: IO ()
testDatum = do
  writeData "datum.json" saleEx
  putStrLn "Done"

writeData :: PlutusTx.ToData a => FilePath -> a -> IO ()
writeData file isData = LB.writeFile file (toJsonString isData)

toJsonString :: PlutusTx.ToData a => a -> LB.ByteString
toJsonString =
  Json.encode
    . scriptDataToJson ScriptDataJsonDetailedSchema
    . fromPlutusData
    . PlutusTx.toData
