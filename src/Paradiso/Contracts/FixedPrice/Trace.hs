{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NumericUnderscores #-}
{-# LANGUAGE OverloadedStrings #-}

module Paradiso.Contracts.FixedPrice.Trace
    ( test
    ) where

import Plutus.Trace.Emulator as Emulator
    ( activateContractWallet, waitNSlots, runEmulatorTraceIO', callEndpoint, EmulatorConfig(..) )
import           Control.Monad    (void)
import           PlutusTx.Prelude as Plutus ( ($), (<>), Either(..) )
import           Ledger.Value     as Value (singleton)
import qualified Data.Map         as Map
import qualified Ledger.Ada       as Ada

import           Prelude      (IO)
import           Data.Default (def)

import Paradiso.Contracts.FixedPrice.Utility  (wallet)
import Paradiso.Contracts.FixedPrice.OffChain (endpoints)
import Paradiso.Contracts.FixedPrice.Types    (StartRequest(..), BuyRequest(..))

startRequest1 :: StartRequest
startRequest1 = StartRequest
    { oAssetCurrency    = "88"
    , oAssetName        = "PalmTree01"
    , oPrice  = 80_000_000
    }

buyRequest1 :: BuyRequest
buyRequest1 = BuyRequest
    { bAssetCurrency = oAssetCurrency startRequest1
    , bAssetName = oAssetName startRequest1
    }

test :: IO ()
test = do
    let dist = Map.fromList [ (wallet 1, Ada.lovelaceValueOf 0)
                            , (wallet 2, Ada.lovelaceValueOf 100_000_000)
                            , (wallet 3, Ada.lovelaceValueOf 10_000_000
                                      <> Value.singleton (oAssetCurrency startRequest1) (oAssetName startRequest1) 1)
                            , (wallet 4, Ada.lovelaceValueOf 0)
                            ]
        emCfg = EmulatorConfig (Left dist) def def
    runEmulatorTraceIO' def emCfg $ do
        h2 <- activateContractWallet (wallet 2) endpoints
        h3 <- activateContractWallet (wallet 3) endpoints
        void $ Emulator.waitNSlots 1
        callEndpoint @"start" h3 startRequest1
        void $ Emulator.waitNSlots 1
        callEndpoint @"buy" h2 buyRequest1
        void $ Emulator.waitNSlots 1
