{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE NumericUnderscores         #-}

module Paradiso.Contracts.FixedPrice.OnChain
    ( fixedPriceScriptSerialised
    , fixedPriceScriptAsShortBs
    , typedFixedPriceValidator
    , FixedPricing
    , fixedPriceValidator
    , fixedPriceValidatorHash
    , maybeGetOutputDatum
    ) where

import qualified Data.ByteString.Lazy  as LB
import qualified Data.ByteString.Short as SBS
import           Codec.Serialise       ( serialise )

import           Cardano.Api.Shelley (PlutusScript (..), PlutusScriptV1)

import qualified PlutusTx

import PlutusTx.Trace (traceIfFalse, traceError)
import PlutusTx.Maybe (isJust)

import PlutusTx.Prelude as Plutus ( Bool(..), Eq((==)), (/=), (.), (||), length, (&&), Integer, Maybe(..), (<=), (>=), (>), (+), (*), ($), (-), divide, filter, max)

import Ledger
    ( PubKeyHash(..),
      ValidatorHash,
      validatorHash,
      DatumHash,
      Datum(..),
      txOutDatum,
      txSignedBy,
      ScriptContext(scriptContextTxInfo),
      TxInfo,
      Validator,
      TxOut,
      txInfoSignatories,
      unValidatorScript,
      txInInfoResolved,
      txInfoInputs,
      valuePaidTo,
      txOutAddress,
      findDatum,
      txOutValue,
      txOutDatum,
      findOwnInput)

import qualified Ledger.Typed.Scripts as Scripts
import qualified Plutus.V1.Ledger.Scripts as Plutus
import qualified Plutus.V1.Ledger.Ada as Ada (fromValue, getLovelace)

import Plutus.V1.Ledger.Value as Value (assetClassValueOf, valueOf, Value, AssetClass(..))
import Plutus.V1.Ledger.Address as Address (toValidatorHash)
import Plutus.V1.Ledger.Contexts as Contexts (getContinuingOutputs)

import Paradiso.Contracts.FixedPrice.Types    (FixedPriceSale(..), FixedPriceAction(..), ContractParams(..))

{-# INLINABLE isValidPrice #-}
isValidPrice :: Integer -> Bool
isValidPrice p = p > 0 && p <= 10_000_000_000_000

{-# INLINABLE isValidRoyalty #-}
isValidRoyalty :: Integer -> Bool
isValidRoyalty p = p >= 0 && p <= 500_000

{-# INLINABLE computeLovelaces #-}
computeLovelaces :: Integer -> Integer -> Integer
computeLovelaces am p = divide (am * p) 1_000_000

{-# INLINABLE computeLovelacesWithMin #-}
computeLovelacesWithMin :: Integer -> Integer -> Integer
computeLovelacesWithMin am p = max (computeLovelaces am p) 1_000_000

{-# INLINABLE computeRoyaltyLovelaces #-}
computeRoyaltyLovelaces :: Integer -> Integer -> Integer
computeRoyaltyLovelaces am p = case p > 0 of
        True -> computeLovelacesWithMin am p
        False -> 0

{-# INLINABLE maybeGetOutputDatum #-}
maybeGetOutputDatum :: TxOut -> (DatumHash -> Maybe Datum) -> Maybe FixedPriceSale
maybeGetOutputDatum o f = do
    dh      <- txOutDatum o
    Datum d <- f dh
    PlutusTx.fromBuiltinData d
        
{-# INLINABLE getOutputDatum #-}
getOutputDatum :: TxOut -> (DatumHash -> Maybe Datum) -> FixedPriceSale
getOutputDatum o f = case maybeGetOutputDatum o f of
    Nothing -> traceError "unable to get sale datum"
    Just od -> od

{-# INLINABLE mkFixedPriceValidator #-}
mkFixedPriceValidator :: ContractParams -> FixedPriceSale -> FixedPriceAction -> ScriptContext -> Bool
mkFixedPriceValidator contractSettings inputDatum r ctx =
    let
        cs = (assetCurrency inputDatum)
        tn = (assetName inputDatum)
        sellerHash = (sellerPubKeyHash inputDatum)

        info :: TxInfo
        info = scriptContextTxInfo ctx

        ownInput :: TxOut
        ownInput = case findOwnInput ctx of
            Nothing -> traceError "asset input missing"
            Just i  -> txInInfoResolved i

        hasOneScriptInput :: Bool
        hasOneScriptInput =
            let
                xs = filter (isJust . toValidatorHash . txOutAddress . txInInfoResolved) $ txInfoInputs info
            in
                length xs == 1

        inputHasToken :: Bool
        inputHasToken = assetClassValueOf (txOutValue ownInput) (AssetClass(cs, tn)) == 1

        containsToken :: Value.Value -> Bool
        containsToken v = (valueOf v cs tn) >= 1

        isSignedBySeller :: Bool
        isSignedBySeller = txSignedBy info sellerHash
    in
        traceIfFalse "expected exactly one script input" hasOneScriptInput &&
        traceIfFalse "expected input has correct token" inputHasToken &&
        case r of
            Buy ->
                let
                    marketHash = (marketPubKeyHash contractSettings)
                    marketRate = (marketPercentage contractSettings)
                    royaltyHash = (royaltyPubKeyHash inputDatum)
                    royaltyRate = (royaltyPercentage inputDatum)
                    saleTotal = (price inputDatum)

                    amountNeededByMarket = (computeLovelacesWithMin saleTotal marketRate)

                    amountNeededByRoyalty = case royaltyHash /= sellerHash of
                        True -> (computeRoyaltyLovelaces saleTotal royaltyRate)
                        False -> 0

                    feeAmount = amountNeededByMarket + amountNeededByRoyalty

                    amountNeededBySeller = saleTotal - feeAmount

                    amountPaidToMarket = Ada.getLovelace (Ada.fromValue (Ledger.valuePaidTo info marketHash))
                    amountPaidToSeller = Ada.getLovelace (Ada.fromValue (Ledger.valuePaidTo info sellerHash))

                    isRoyaltyPaid :: Bool
                    isRoyaltyPaid = amountNeededByRoyalty <= 0 || (Ada.getLovelace (Ada.fromValue (Ledger.valuePaidTo info royaltyHash)) >= amountNeededByRoyalty)

                    isCorrectSplit :: Bool
                    isCorrectSplit = 
                        amountPaidToMarket >= amountNeededByMarket &&
                        amountPaidToSeller >= amountNeededBySeller &&
                        isRoyaltyPaid

                    signer :: PubKeyHash
                    signer = case txInfoSignatories info of
                        [pubKeyHash] -> pubKeyHash
                        _            -> traceError "invalid signatory"

                    isBuyerNonSeller :: Bool
                    isBuyerNonSeller = signer /= sellerHash

                    buyerGetsToken :: Bool
                    buyerGetsToken = containsToken (valuePaidTo info signer)

                    isValidPriceBuy :: Bool
                    isValidPriceBuy = isValidPrice saleTotal

                    isValidRoyaltyBuy :: Bool
                    isValidRoyaltyBuy = isValidRoyalty royaltyRate

                    isSellerNonMarket :: Bool
                    isSellerNonMarket = sellerHash /= marketHash

                    isRoyaltyNonMarket :: Bool
                    isRoyaltyNonMarket = royaltyHash /= marketHash
                in
                    traceIfFalse "expected non-market seller address" isSellerNonMarket &&
                    traceIfFalse "expected non-market royalty address" isRoyaltyNonMarket &&
                    traceIfFalse "expected positive price up to 10T lovelace" isValidPriceBuy &&
                    traceIfFalse "expected max royalty amount up to 50 percent" isValidRoyaltyBuy &&
                    traceIfFalse "buyer must not also be seller" isBuyerNonSeller &&
                    traceIfFalse "incorrect distribution of funds" isCorrectSplit &&
                    traceIfFalse "expected buyer to get token" buyerGetsToken

            Update ->
                let
                    ownOutput :: TxOut
                    ownOutput = case getContinuingOutputs ctx of
                        [o] -> o
                        _   -> traceError "expected exactly one token output"

                    scriptGetsToken :: Bool
                    scriptGetsToken = assetClassValueOf (txOutValue ownOutput) (AssetClass(cs, tn)) == 1

                    outputDatum = getOutputDatum ownOutput (`findDatum` info)

                    isValidPriceChange :: Bool
                    isValidPriceChange = isValidPrice (price outputDatum) && (price outputDatum) /= (price inputDatum)

                    isCurrencySymbolUnchanged:: Bool
                    isCurrencySymbolUnchanged = (assetCurrency outputDatum) == (assetCurrency inputDatum)

                    isTokenNameUnchanged:: Bool
                    isTokenNameUnchanged = (assetName outputDatum) == (assetName inputDatum)

                    isSellerAddressUnchanged:: Bool
                    isSellerAddressUnchanged = (sellerPubKeyHash outputDatum) == (sellerPubKeyHash inputDatum)

                    isRoyaltyAddressUnchanged:: Bool
                    isRoyaltyAddressUnchanged = (royaltyPubKeyHash outputDatum) == (royaltyPubKeyHash inputDatum)

                    isRoyaltyPercentageUnchanged:: Bool
                    isRoyaltyPercentageUnchanged = (royaltyPercentage outputDatum) == (royaltyPercentage inputDatum)
                in
                    traceIfFalse "expected positive price up to 10T lovelace" isValidPriceChange &&
                    traceIfFalse "must not change currency symbol" isCurrencySymbolUnchanged &&
                    traceIfFalse "must not change token name" isTokenNameUnchanged &&
                    traceIfFalse "must not change seller address" isSellerAddressUnchanged &&
                    traceIfFalse "must not change royalty address" isRoyaltyAddressUnchanged &&
                    traceIfFalse "must not change royalty percentage" isRoyaltyPercentageUnchanged &&
                    traceIfFalse "must be signed by seller" isSignedBySeller &&
                    traceIfFalse "must include output token" scriptGetsToken

            Cancel ->
                let
                    sellerGetsToken :: Bool
                    sellerGetsToken = containsToken (valuePaidTo info sellerHash)
                in
                    traceIfFalse "must be signed by seller" isSignedBySeller &&
                    traceIfFalse "seller must get token" sellerGetsToken

data FixedPricing
instance Scripts.ValidatorTypes FixedPricing where
    type instance DatumType FixedPricing = FixedPriceSale
    type instance RedeemerType FixedPricing = FixedPriceAction

typedFixedPriceValidator :: ContractParams -> Scripts.TypedValidator FixedPricing
typedFixedPriceValidator p = Scripts.mkTypedValidator @FixedPricing
    ($$(PlutusTx.compile [|| mkFixedPriceValidator ||])
    `PlutusTx.applyCode` PlutusTx.liftCode p)
    $$(PlutusTx.compile [|| wrap ||])
  where
    wrap = Scripts.wrapValidator @FixedPriceSale @FixedPriceAction

fixedPriceValidator :: ContractParams -> Validator
fixedPriceValidator = Scripts.validatorScript . typedFixedPriceValidator

fixedPriceValidatorHash :: ContractParams -> ValidatorHash
fixedPriceValidatorHash = validatorHash . fixedPriceValidator

fixedPriceScript :: ContractParams -> Plutus.Script
fixedPriceScript = Ledger.unValidatorScript . fixedPriceValidator

fixedPriceScriptAsShortBs :: ContractParams -> SBS.ShortByteString
fixedPriceScriptAsShortBs = SBS.toShort . LB.toStrict . serialise . fixedPriceScript

fixedPriceScriptSerialised :: ContractParams -> PlutusScript PlutusScriptV1
fixedPriceScriptSerialised = PlutusScriptSerialised . fixedPriceScriptAsShortBs
