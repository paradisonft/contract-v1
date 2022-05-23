{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE NumericUnderscores #-}

module Paradiso.Contracts.FixedPrice.Utility
    ( walletPubKeyHash
    , wallet
    , authorPkhLocal
    , companyPkhLocal
    , companyPkhTestnet
    , companyPkhMainnet
    , localSettings
    , testnetSettings
    , mainnetSettings ) where

import           Plutus.V1.Ledger.Crypto (PubKeyHash)
import           Wallet.Emulator.Wallet (Wallet, knownWallet, walletPubKeyHash)

import           Prelude hiding ((.))

import Paradiso.Contracts.FixedPrice.Types  (ContractParams(..))

wallet :: Integer -> Wallet
wallet = knownWallet

companyPkhLocal :: PubKeyHash
companyPkhLocal = walletPubKeyHash $ wallet 1

authorPkhLocal :: PubKeyHash
authorPkhLocal = walletPubKeyHash $ wallet 4

localSettings :: ContractParams
localSettings = ContractParams { marketPubKeyHash = companyPkhLocal, marketPercentage = 20_000 }

companyPkhTestnet :: PubKeyHash
companyPkhTestnet = "f357e0eb0580a6605609eb6ed20ac60ea6c387447329af1b75cc34c1"

testnetSettings :: ContractParams
testnetSettings = ContractParams { marketPubKeyHash = companyPkhTestnet, marketPercentage = 20_000 }

companyPkhMainnet :: PubKeyHash
companyPkhMainnet = "5bf54f0c44b05f1144f337f48c3b28a65206e275f56314b9d23f5cf5"

mainnetSettings :: ContractParams
mainnetSettings = ContractParams { marketPubKeyHash = companyPkhMainnet, marketPercentage = 20_000 }