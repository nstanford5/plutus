{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE KindSignatures     #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module PlutusTx.Blueprint.Validator where

import Prelude

import Data.Aeson (ToJSON (..), (.=))
import Data.Aeson qualified as Aeson
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as Base16
import Data.Kind (Type)
import Data.List.NonEmpty (NonEmpty)
import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text.Encoding qualified as Text
import PlutusTx.Blueprint.Argument (ArgumentBlueprint)
import PlutusTx.Blueprint.Parameter (ParameterBlueprint)

{- | A blueprint of a validator, as defined by the CIP-0057

  The 'referencedTypes' phantom type parameter is used to track the types used in the contract
  making sure their schemas are included in the blueprint and that they are referenced
  in a type-safe way.
-}
data ValidatorBlueprint (referencedTypes :: [Type]) = MkValidatorBlueprint
  { validatorTitle        :: Text
  -- ^ A short and descriptive name for the validator.
  , validatorDescription  :: Maybe Text
  -- ^ An informative description of the validator.
  , validatorRedeemer     :: ArgumentBlueprint referencedTypes
  -- ^ A description of the redeemer format expected by this validator.
  , validatorDatum        :: Maybe (ArgumentBlueprint referencedTypes)
  -- ^ A description of the datum format expected by this validator.
  , validatorParameters   :: Maybe (NonEmpty (ParameterBlueprint referencedTypes))
  -- ^ A list of parameters required by the script.
  , validatorCompiledCode :: Maybe CompiledCode
  -- ^ A compiled contract code with its hash.
  }
  deriving stock (Show, Eq)

instance ToJSON (ValidatorBlueprint referencedTypes) where
  toJSON MkValidatorBlueprint{..} =
    Aeson.object
      $ catMaybes
        [ Just $ "title" .= validatorTitle
        , ("description" .=) <$> validatorDescription
        , Just $ "redeemer" .= validatorRedeemer
        , ("datum" .=) <$> validatorDatum
        , ("parameters" .=) <$> validatorParameters
        , ("compiledCode" .=) . toHex . compiledCodeBytes <$> validatorCompiledCode
        , ("hash" .=) . toHex . compiledCodeHash <$> validatorCompiledCode
        ]
   where
    toHex :: ByteString -> Text
    toHex = Text.decodeUtf8 . Base16.encode

data CompiledCode = MkCompiledCode
  { compiledCodeBytes :: ByteString
  -- ^ A full compiled and CBOR-encoded serialized flat script.
  , compiledCodeHash  :: ByteString
  -- ^ A blake2b-224 hash digest of the validator script, as found in addresses.
  }
  deriving stock (Show, Eq)
