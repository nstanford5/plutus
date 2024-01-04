{-# LANGUAGE NoImplicitPrelude #-}

module PlutusLedgerApi.Data.V2.Contexts where

import PlutusLedgerApi.Data.V1.Value
import PlutusLedgerApi.V1.Address (Address (..))
import PlutusLedgerApi.V1.Contexts (ScriptPurpose (..))
import PlutusLedgerApi.V1.Credential (Credential (..), StakingCredential)
import PlutusLedgerApi.V1.Crypto (PubKeyHash (..))
import PlutusLedgerApi.V1.DCert (DCert (..))
import PlutusLedgerApi.V1.Scripts
import PlutusLedgerApi.V1.Time (POSIXTimeRange)
import PlutusLedgerApi.V2.Contexts hiding (ScriptContext (..), TxInfo (..))
import PlutusLedgerApi.V2.Tx (TxId (..), TxOut (..), TxOutRef (..))
import PlutusTx
import PlutusTx.DataMap (Map)
import PlutusTx.DataMap qualified as Map
import PlutusTx.Prelude

import GHC.Generics (Generic)
import Prelude qualified as Haskell

-- | A pending transaction. This is the view as seen by validator scripts, so some details are stripped out.
data TxInfo = TxInfo
    { txInfoInputs          :: [TxInInfo] -- ^ Transaction inputs; cannot be an empty list
    , txInfoReferenceInputs :: [TxInInfo] -- ^ /Added in V2:/ Transaction reference inputs
    , txInfoOutputs         :: [TxOut] -- ^ Transaction outputs
    , txInfoFee             :: Value -- ^ The fee paid by this transaction.
    , txInfoMint            :: Value -- ^ The 'Value' minted by this transaction.
    , txInfoDCert           :: [DCert] -- ^ Digests of certificates included in this transaction
    , txInfoWdrl            :: Map StakingCredential Integer -- ^ Withdrawals
                                                      -- /V1->V2/: changed from assoc list to a 'PlutusTx.AssocMap'
    , txInfoValidRange      :: POSIXTimeRange -- ^ The valid range for the transaction.
    , txInfoSignatories     :: [PubKeyHash] -- ^ Signatures provided with the transaction, attested that they all signed the tx
    , txInfoRedeemers       :: Map ScriptPurpose Redeemer -- ^ /Added in V2:/ a table of redeemers attached to the transaction
    , txInfoData            :: Map DatumHash Datum -- ^ The lookup table of datums attached to the transaction
                                                  -- /V1->V2/: changed from assoc list to a 'PlutusTx.AssocMap'
    , txInfoId              :: TxId  -- ^ Hash of the pending transaction body (i.e. transaction excluding witnesses)
    } deriving stock (Generic, Haskell.Show, Haskell.Eq)

-- | The context that the currently-executing script can access.
data ScriptContext = ScriptContext
    { scriptContextTxInfo  :: TxInfo -- ^ information about the transaction the currently-executing script is included in
    , scriptContextPurpose :: ScriptPurpose -- ^ the purpose of the currently-executing script
    }
    deriving stock (Generic, Haskell.Eq, Haskell.Show)
