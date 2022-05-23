{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeOperators         #-}

module Paradiso.Contracts.FixedPrice.Types
    ( FixedPriceAction (..)
    , FixedPriceSchema
    , StartRequest (..)
    , BuyRequest (..)
    , UpdateRequest (..)
    , CancelRequest (..)
    , FixedPriceSale (..)
    , ContractParams (..)
    )
    where

import           Data.Aeson                (ToJSON, FromJSON)
import           GHC.Generics              (Generic)
import           Prelude                   (Show (..))
import qualified Prelude                   as Pr

import           Schema                    (ToSchema)
import qualified PlutusTx
import           PlutusTx.Prelude          as Plutus ( Eq(..), (&&), Integer )
import           Ledger                    ( TokenName, CurrencySymbol, PubKeyHash )
import           Plutus.Contract           ( Endpoint, type (.\/) )

data ContractParams 
    = ContractParams
        { marketPubKeyHash      :: !PubKeyHash
        , marketPercentage      :: !Plutus.Integer
        }
         deriving (Generic, ToJSON, FromJSON)

PlutusTx.makeIsDataIndexed ''ContractParams [('ContractParams, 0)]
PlutusTx.makeLift ''ContractParams

data FixedPriceSale
    = FixedPriceSale
        { sellerPubKeyHash      :: !PubKeyHash
        , assetCurrency         :: !CurrencySymbol
        , assetName             :: !TokenName
        , price                 :: !Plutus.Integer
        , royaltyPubKeyHash     :: !PubKeyHash
        , royaltyPercentage     :: !Plutus.Integer
        } deriving (Generic, ToJSON, FromJSON)

instance Eq FixedPriceSale where
    {-# INLINABLE (==) #-}
    a == b = (sellerPubKeyHash      a == sellerPubKeyHash       b) &&
             (assetCurrency         a == assetCurrency          b) &&
             (assetName             a == assetName              b) &&
             (price                 a == price                  b) &&
             (royaltyPubKeyHash     a == royaltyPubKeyHash      b) &&
             (royaltyPercentage     a == royaltyPercentage      b)

PlutusTx.makeIsDataIndexed ''FixedPriceSale [('FixedPriceSale, 0)]
PlutusTx.makeLift ''FixedPriceSale

data FixedPriceAction = Buy | Update | Cancel
    deriving Show

PlutusTx.makeIsDataIndexed ''FixedPriceAction [('Buy, 0), ('Update, 1), ('Cancel, 2)]
PlutusTx.makeLift ''FixedPriceAction

data StartRequest = StartRequest
    { oAssetCurrency         :: !CurrencySymbol
    , oAssetName             :: !TokenName
    , oPrice                 :: !Plutus.Integer
    } deriving (Pr.Eq, Pr.Ord, Show, Generic, ToJSON, FromJSON, ToSchema)

data BuyRequest = BuyRequest
    { bAssetCurrency         :: !CurrencySymbol
    , bAssetName             :: !TokenName
    } deriving (Pr.Eq, Pr.Ord, Show, Generic, ToJSON, FromJSON, ToSchema)

data UpdateRequest = UpdateRequest
    { uAssetCurrency         :: !CurrencySymbol
    , uAssetName             :: !TokenName
    , uPrice                 :: !Plutus.Integer
    } deriving (Pr.Eq, Pr.Ord, Show, Generic, ToJSON, FromJSON, ToSchema)

data CancelRequest = CancelRequest
    { cAssetCurrency         :: !CurrencySymbol
    , cAssetName             :: !TokenName
    } deriving (Pr.Eq, Pr.Ord, Show, Generic, ToJSON, FromJSON, ToSchema)

type FixedPriceSchema  = Endpoint "start" StartRequest
                        .\/
                        Endpoint "buy" BuyRequest
                        .\/
                        Endpoint "update" UpdateRequest
                        .\/
                        Endpoint "cancel" CancelRequest
