{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE NamedFieldPuns    #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE ViewPatterns      #-}
module Dogfooding where

import Data.Aeson qualified as J
import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as BS
import Data.Text qualified as T
import PlutusLedgerApi.V1.Interval qualified as Interval
import PlutusLedgerApi.V3
import PlutusTx
import PlutusTx.AssocMap qualified as AM
import PlutusTx.Prelude
import Prelude qualified as H
import Text.Hex qualified as Hex

checkParameterPredicate :: AM.Map BuiltinByteString Integer -> BuiltinString -> (Integer -> Bool) -> Bool
checkParameterPredicate changes name p = case AM.lookup (encodeUtf8 name) changes of
  Just i  -> p i
  Nothing -> True

checkTxFeePerByte :: AM.Map BuiltinByteString Integer -> Bool
checkTxFeePerByte changes = checkParameterPredicate changes "txFeePerByte" $ \i ->
  if | i < 30    -> trace "txFeePerByte is less than 30" False
     | i > 1000  -> trace "txFeePerByte is greater than 1000" False
     | otherwise -> True

checkCollateralPercentage :: AM.Map BuiltinByteString Integer -> Bool
checkCollateralPercentage changes = checkParameterPredicate changes "collateralPercentage" $ \i ->
  if | i < 0     -> trace "collateralPercentage is negative" False
     | otherwise -> True

validator :: BuiltinData -> ScriptContext -> Bool
validator _ ScriptContext{scriptContextTxInfo=TxInfo{txInfoValidRange, txInfoProposalProcedures=procedures}, scriptContextPurpose=Proposing _} = all checkProcedure procedures
  where
    checkProcedure procedure =
      case procedure of
        ProposalProcedure{ppGovernanceAction=ParameterChange _ (ChangedParameters (unsafeFromBuiltinData -> changes))} ->
          checkTxFeePerByte changes && checkTxFeePerByte changes && checkCollateralPercentage changes
        ProposalProcedure{ppGovernanceAction=TreasuryWithdrawals _} ->
          2147483648 `Interval.before` txInfoValidRange
        -- only care about parameter changes and withdrawals
        _ -> True
-- Not a governance proposal, unclear what we should be doing here
validator _ _ = True

-----

wrapValidator :: (UnsafeFromData r) => (r -> ScriptContext -> Bool) -> BuiltinData -> BuiltinData -> ()
wrapValidator v (unsafeFromBuiltinData -> r) (unsafeFromBuiltinData -> c) = check (v r c)

wrappedValidator :: BuiltinData -> BuiltinData -> ()
wrappedValidator = wrapValidator validator

compiledValidator :: CompiledCode (BuiltinData -> BuiltinData -> ())
compiledValidator = $$(compile [|| wrappedValidator ||])

serialisedValidator :: SerialisedScript
serialisedValidator = serialiseCompiledCode compiledValidator

validatorHex :: T.Text
validatorHex = Hex.encodeHex $ BS.fromShort serialisedValidator

envelopeObject :: J.Value
envelopeObject = J.object [ "type" J..= ("PlutusScriptV3" :: T.Text), "description" J..= ("constitutionScript" :: T.Text), "cborHex" J..= validatorHex]

dumpValidator :: H.IO ()
dumpValidator = H.print $ J.encode envelopeObject
