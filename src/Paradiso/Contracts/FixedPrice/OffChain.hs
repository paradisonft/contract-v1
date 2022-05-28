{-# LANGUAGE NumericUnderscores         #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE NumericUnderscores         #-}

module Paradiso.Contracts.FixedPrice.OffChain
    ( endpoints
    )
    where

import qualified Data.Map                  as Map

import           Data.Monoid               as Mnd ( (<>) )

import           Control.Monad             ( void, forever )
import           Data.Aeson                (ToJSON)
import           Data.Text                 (Text)
import           Prelude                   (String)

import Plutus.Contract as Contract
    ( AsContractError,
      logError,
      logInfo,
      awaitTxConfirmed,
      endpoint,
      ownPubKeyHash,
      submitTxConstraintsWith,
      utxosAt,
      utxosTxOutTxAt,
      handleError,
      select,
      Contract,
      Promise(awaitPromise) )
import qualified PlutusTx
import PlutusTx.Prelude as Plutus
    ( return, Bool, Maybe(..), Eq((==)), (<$>), ($) )
import Ledger
    ( scriptAddress,
      getCardanoTxId,
      pubKeyHashAddress,
      CurrencySymbol,
      TokenName,
      Redeemer(Redeemer),
      TxOut(txOutValue),
      TxOutRef,
      ChainIndexTxOut, toTxOut )
import Ledger.Constraints as Constraints
    ( otherScript,
      typedValidatorLookups,
      unspentOutputs,
      mustPayToPubKey,
      mustPayToTheScript,
      mustSpendScriptOutput )
import Ledger.Value as Value
    ( singleton,
      valueOf )
import qualified Plutus.V1.Ledger.Ada as Ada (lovelaceValueOf)
import Plutus.ChainIndex.Tx ( ChainIndexTx(_citxData) )

import           Paradiso.Contracts.FixedPrice.Types               (ContractParams(..), FixedPriceSale(..), FixedPriceAction(..), FixedPriceSchema, StartRequest(..), BuyRequest(..), UpdateRequest(..), CancelRequest(..))
import           Paradiso.Contracts.FixedPrice.OnChain             as O2 (FixedPricing, typedFixedPriceValidator, fixedPriceValidator, maybeGetOutputDatum)
import           Paradiso.Contracts.FixedPrice.Utility             (localSettings)

startSale :: StartRequest -> Contract w FixedPriceSchema Text ()
startSale sr = do

    pkh <- Contract.ownPubKeyHash
    utxos <- utxosAt (pubKeyHashAddress pkh)

    let val         = Value.singleton (oAssetCurrency sr) (oAssetName sr) 1
        
        saleNft    = FixedPriceSale { 
                    sellerPubKeyHash        = pkh
                    , assetCurrency         = (oAssetCurrency sr)
                    , assetName             = (oAssetName sr)
                    , price                 = (oPrice sr)
                    , royaltyPubKeyHash     = pkh
                    , royaltyPercentage     = 0
                }
        
        lookups = Constraints.unspentOutputs utxos <>
                  Constraints.typedValidatorLookups (O2.typedFixedPriceValidator localSettings)

        tx      = Constraints.mustPayToTheScript saleNft val

    ledgerTx <- submitTxConstraintsWith @O2.FixedPricing lookups tx
    void $ awaitTxConfirmed $ getCardanoTxId ledgerTx

    Contract.logInfo @String "startSale transaction confirmed"

buy :: BuyRequest -> Contract w FixedPriceSchema Text ()
buy br = do
    pkh <- Contract.ownPubKeyHash
    sale <- findSale (bAssetCurrency br, bAssetName br)
    case sale of
        Nothing -> Contract.logError @String "No sale found"
        Just (oref, o, saleData) -> do

            let r       = Redeemer $ PlutusTx.toBuiltinData Buy

                val     = Value.singleton (assetCurrency saleData) (assetName saleData) 1

                valAdaS = Ada.lovelaceValueOf 78_400_000
                valAdaF = Ada.lovelaceValueOf 1_600_000

            let lookups = Constraints.typedValidatorLookups (O2.typedFixedPriceValidator localSettings) <>
                          Constraints.unspentOutputs (Map.singleton oref o) <>
                          Constraints.otherScript (O2.fixedPriceValidator localSettings)

                tx      = Constraints.mustSpendScriptOutput oref r <>
                          Constraints.mustPayToPubKey pkh val <>
                          Constraints.mustPayToPubKey (sellerPubKeyHash saleData) valAdaS <>
                          Constraints.mustPayToPubKey (marketPubKeyHash localSettings) valAdaF

            ledgerTx <- submitTxConstraintsWith lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx
            Contract.logInfo @String "buy transaction confirmed"

update :: UpdateRequest -> Contract w FixedPriceSchema Text ()
update ur = do
    sale <- findSale (uAssetCurrency ur, uAssetName ur)
    case sale of
        Nothing -> Contract.logError @String "No sale found"
        Just (oref, o, saleData) -> do

            let r       = Redeemer $ PlutusTx.toBuiltinData Update

                val     = Value.singleton (assetCurrency saleData) (assetName saleData) 1

                saleData'   = saleData { price = (uPrice ur) }

                lookups = Constraints.typedValidatorLookups (O2.typedFixedPriceValidator localSettings) <>
                          Constraints.otherScript (O2.fixedPriceValidator localSettings) <>
                          Constraints.unspentOutputs (Map.singleton oref o)

                tx      = Constraints.mustSpendScriptOutput oref r <>
                          Constraints.mustPayToTheScript saleData' val

            ledgerTx <- submitTxConstraintsWith lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx

            Contract.logInfo @String "Price updated"

cancelSale :: CancelRequest -> Contract w FixedPriceSchema Text ()
cancelSale cr = do
    sale <- findSale (cAssetCurrency cr, cAssetName cr)
    case sale of
        Nothing -> Contract.logError @String "No sale found"
        Just (oref, o, saleData) -> do
            let r       = Redeemer $ PlutusTx.toBuiltinData Cancel

                val     = Value.singleton (assetCurrency saleData) (assetName saleData) 1

                lookups = Constraints.typedValidatorLookups (O2.typedFixedPriceValidator localSettings) <>
                          Constraints.otherScript (O2.fixedPriceValidator localSettings) <>
                          Constraints.unspentOutputs (Map.singleton oref o)

                tx      = Constraints.mustSpendScriptOutput oref r <>
                          Constraints.mustPayToPubKey (sellerPubKeyHash saleData) val

            ledgerTx <- submitTxConstraintsWith lookups tx
            void $ awaitTxConfirmed $ getCardanoTxId ledgerTx

            Contract.logInfo @String "close transaction confirmed"

findSale :: (AsContractError e, ToJSON e) => (CurrencySymbol, TokenName) -> Contract w FixedPriceSchema e (Maybe (TxOutRef, ChainIndexTxOut, FixedPriceSale))
findSale (cs, tn) = do
    utxos <- Map.filter f <$> utxosTxOutTxAt (scriptAddress $ O2.fixedPriceValidator localSettings)
    return $ case Map.toList utxos of
        [(oref, (o, citx))] -> do
            oDatum <- maybeGetOutputDatum (toTxOut o) $ \dh -> Map.lookup dh $ _citxData citx
            Just (oref, o, oDatum)
        _           -> Nothing

  where
    f :: (ChainIndexTxOut, Plutus.ChainIndex.Tx.ChainIndexTx) -> Bool
    f (o, _) = valueOf (txOutValue $ toTxOut o) cs tn == 1

endpoints :: Contract () FixedPriceSchema Text ()
endpoints = forever
          $ handleError logError
          $ awaitPromise
          $ start' `select` buy'
                   `select` update'
                   `select` cancel'
  where
    start'          = endpoint @"start"          $ \or           -> startSale or
    buy'            = endpoint @"buy"            $ \br           -> buy br
    update'         = endpoint @"update"         $ \ur           -> update ur
    cancel'         = endpoint @"cancel"         $ \cr           -> cancelSale cr
