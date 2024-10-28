{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Azure.Secret
    ( getSecret
    , getSecretEither
    ) where

import Data.Data (Proxy (..))
import Data.Text (Text)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.API (Capture, Get, Header', JSON, QueryParam', Required, Strict, (:>))
import Servant.Client (BaseUrl (..), ClientM, Scheme (..), client, mkClientEnv, runClientM)
import UnliftIO (MonadIO (..), throwString)

import Azure.Secret.Types (KeyVaultHost (..), KeyVaultResponse (..), SecretName (..))
import Azure.Types (AccessToken (..))

import qualified Data.Text as Text

{- | Fetches a secret from Key vault.

All errors are thrown in IO. For variant where error is
caught in a @Left@ branch, see @getSecretEither@
-}
getSecret ::
    MonadIO m =>
    -- | Name of the secret
    SecretName ->
    -- | Host to identify the key vault. This is where all the request will be
    -- made. It is of the form @{keyvault-name}.vault.azure.net@
    KeyVaultHost ->
    -- | Access token which will form part of the authentication header
    AccessToken ->
    m KeyVaultResponse
getSecret secretName host accessToken = do
    res <- getSecretEither secretName host accessToken
    case res of
        Left err -> throwString $ show err
        Right value -> pure value

{- | Fetches a secret from key vault provided the name of the secret and the key vault host.
These secrets can be found under @/secrets@ path under key vault section in Azure portal.
-}
getSecretEither ::
    MonadIO m =>
    -- | Name of the secret
    SecretName ->
    -- | Host to identify the key vault. This is where all the request will be
    -- made. It is of the form @{keyvault-name}.vault.azure.net@
    KeyVaultHost ->
    -- | Access token which will form part of the authentication header
    AccessToken ->
    m (Either Text KeyVaultResponse)
getSecretEither = callKeyVaultClient getSecretFromVault

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
    SecretName ->
    KeyVaultHost ->
    AccessToken ->
    m (Either Text KeyVaultResponse)
callKeyVaultClient action (SecretName secretName) (KeyVaultHost vaultHost) accessToken = do
    manager <- liftIO newTlsManager
    res <-
        liftIO $
            runClientM
                (action secretName 7.4 ("Bearer " <> atAccessToken accessToken))
                (mkClientEnv manager $ BaseUrl Https (Text.unpack vaultHost) 443 "")
    pure $ case res of
        Left err ->
            Left . Text.pack $ show err
        Right response ->
            Right response
