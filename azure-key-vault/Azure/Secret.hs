{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Azure.Secret
    ( KeyVaultResponse (..)
    , GetSecretFromVaultApi
    , getSecretFromVault
    , callKeyVaultClient
    ) where

import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Data (Proxy (..))
import Data.Text (Text)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.API (Capture, Get, Header', JSON, QueryParam', Required, Strict, (:>))
import Servant.Client (BaseUrl (..), ClientM, Scheme (..), client, mkClientEnv, runClientM)
import UnliftIO (MonadIO (..), throwIO)
import GHC.Generics (Generic)

import Azure.Auth (defaultAzureCredential)
import Azure.Types (AccessToken (..), Token)

import qualified Data.Text as Text

newtype KeyVaultResponse = KeyVaultResponse
    { unKeyValueReponse :: Text
    }
    deriving stock (Eq, Show, Generic)

instance FromJSON KeyVaultResponse where
    parseJSON = withObject "KeyVaultResponse" $ \o -> do
        unKeyValueReponse <- o .: "value"
        pure KeyVaultResponse{..}

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
    m KeyVaultResponse
callKeyVaultClient action secretName vaultHost tokenStore = do
    manager <- liftIO newTlsManager
    authHeader <- defaultAzureCredential Nothing vaultHost tokenStore
    res <-
        liftIO $
            runClientM
                (action secretName 7.4 ("Bearer " <> atAccessToken authHeader))
                (mkClientEnv manager $ BaseUrl Https (Text.unpack vaultHost) 443 "")
    case res of
        Left err ->
            throwIO err
        Right response ->
            pure response
