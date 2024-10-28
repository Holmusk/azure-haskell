{-# LANGUAGE DeriveGeneric #-}

module Azure.Secret.Types
    ( KeyVaultResponse (..)
    , SecretName (..)
    , KeyVaultHost (..)
    ) where

import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Text (Text)
import GHC.Generics (Generic)

newtype KeyVaultResponse = KeyVaultResponse
    { unKeyValueReponse :: Text
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON KeyVaultResponse where
    parseJSON = withObject "KeyVaultResponse" $ \o -> do
        unKeyValueReponse <- o .: "value"
        pure KeyVaultResponse{..}

newtype SecretName = SecretName {unSecretName :: Text} deriving stock (Eq, Show)

newtype KeyVaultHost = KeyVaultHost {unKeyVaultHost :: Text} deriving stock (Eq, Show)
