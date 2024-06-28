{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Azure.Secret
    ( KeyVaultResponse (..)
    , GetSecretFromVaultApi
    , getSecretFromVault
    , callKeyVaultClient
    , getSecret
    , getSecretEither
    ) where

import Control.Exception (Exception)
import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Data (Proxy (..))
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.API (Capture, Get, Header', JSON, QueryParam', Required, Strict, (:>))
import Servant.Client (BaseUrl (..), ClientError (..), ClientM, Scheme (..), client, mkClientEnv, runClientM)
import UnliftIO (MonadIO (..), throwIO)

import Azure.Auth (defaultAzureCredential)
import Azure.Types (AccessToken (..), Token)

import qualified Data.Text as Text

keyVaultBaseUrl :: Text
keyVaultBaseUrl = "https://vault.azure.net"

newtype KeyVaultResponse = KeyVaultResponse
    { unKeyValueReponse :: Text
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON KeyVaultResponse where
    parseJSON = withObject "KeyVaultResponse" $ \o -> do
        unKeyValueReponse <- o .: "value"
        pure KeyVaultResponse{..}

{- | Fetches a secret from key vault.
All errors are thrown in IO.

For version where errors are returned in a @Left@ branch, use @getSecretEither@
-}
getSecret :: MonadIO m => Text -> Text -> Token -> m KeyVaultResponse
getSecret secretName vaultHost token = do
    secret <- getSecretEither secretName vaultHost token
    case secret of
        Left err -> throwIO err
        Right response -> pure response

getSecretEither :: MonadIO m => Text -> Text -> Token -> m (Either KeyVaultException KeyVaultResponse)
getSecretEither secretName vaultHost token = do
    secret <- callKeyVaultClient getSecretFromVault secretName vaultHost token
    case secret of
        Left err -> pure . Left $ KeyVaultClientError err
        Right response -> pure $ Right response

-- | An exception that can occur when generating an @AccessToken@
data KeyVaultException
    = SecretDoesNotExist !Text
    | KeyVaultClientError !ClientError
    deriving stock (Show, Typeable)

instance Exception KeyVaultException

{-
Path: GET {vaultBaseUrl}/secrets/{secret-name}/{secret-version}?api-version=7.4

secret-version: optional. The latest version will be used by default.
TODO: consider specifying secret-version in the future.
-}
type GetSecretFromVaultApi =
    "secrets"
        :> Capture "secret-name" Text
        :> QueryParam' '[Required, Strict] "api-version" Float
        :> Header' '[Required, Strict] "Authorization" Text
        :> Get '[JSON] KeyVaultResponse

getSecretFromVault :: Text -> Float -> Text -> ClientM KeyVaultResponse
getSecretFromVault = client (Proxy @GetSecretFromVaultApi)

callKeyVaultClient ::
    MonadIO m =>
    (Text -> Float -> Text -> ClientM KeyVaultResponse) ->
    Text ->
    Text ->
    Token ->
    m (Either ClientError KeyVaultResponse)
callKeyVaultClient action secretName vaultHost tokenStore = do
    manager <- liftIO newTlsManager
    authHeader <- defaultAzureCredential Nothing keyVaultBaseUrl tokenStore
    res <-
        liftIO $
            runClientM
                (action secretName 7.4 ("Bearer " <> atAccessToken authHeader))
                (mkClientEnv manager $ BaseUrl Https (Text.unpack vaultHost) 443 "")
    case res of
        Left err ->
            pure $ Left err
        Right response ->
            pure $ Right response
