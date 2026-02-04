{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

{- |
Module      : Azure.Auth
Description : Azure authentication for Haskell applications

This module provides authentication mechanisms for Azure services,
primarily through Managed Identity.

= Basic Usage

@
import Azure.Auth

main :: IO ()
main = do
    cache <- newEmptyTokenCache
    token <- defaultAzureCredential Nothing keyVaultResource cache
    print token
@

= Forcing Token Refresh

If a token has been revoked or you need to force re-authentication:

@
invalidateTokenCache cache
newToken <- defaultAzureCredential Nothing keyVaultResource cache
@
-}
module Azure.Auth
    ( -- * Credential Functions
      defaultAzureCredential
    , withManagedIdentity
    , withManagedIdentityEither

      -- * Exceptions
    , AccessTokenException (..)

      -- * Common Resource URIs
    , keyVaultResource
    , storageResource
    , managementResource

      -- * Re-exports from Azure.Types
    , AccessToken (..)
    , TokenType (..)
    , ResourceUri (..)
    , TokenCache
    , newEmptyTokenCache
    , invalidateTokenCache
    , readTokenCache
    ) where

import Control.Exception (Exception)
import Data.Data (Proxy (..))
import Data.Text (Text)
import Data.Typeable (Typeable)
import Network.HTTP.Client (Manager, defaultManagerSettings, newManager)
import Servant.API (Get, Header', JSON, Optional, QueryParam', Required, Strict, (:>))
import Servant.Client (BaseUrl (..), ClientM, Scheme (..), client, mkClientEnv, runClientM)
import UnliftIO (MonadIO (..), throwIO)
import UnliftIO.Environment (lookupEnv)

import Azure.Types
    ( AccessToken (..)
    , ResourceUri (..)
    , TokenCache
    , TokenType (..)
    , invalidateTokenCache
    , newEmptyTokenCache
    , readTokenCache
    , updateTokenCache
    )
import Azure.Utils (isExpired)

import qualified Data.Text as Text

-- | Resource URI for Azure Key Vault.
keyVaultResource :: ResourceUri
keyVaultResource = ResourceUri "https://vault.azure.net"

-- | Resource URI for Azure Blob Storage.
storageResource :: ResourceUri
storageResource = ResourceUri "https://storage.azure.com"

-- | Resource URI for Azure Resource Manager.
managementResource :: ResourceUri
managementResource = ResourceUri "https://management.azure.com"

{- | IMDS is a REST API that's available at a well-known, non-routable IP address (169.254.169.254).
It is a local-only link that can only be accessed from within the VM.
Communication between the VM and IMDS never leaves the host.
-}
imdsHost :: String
imdsHost = "169.254.169.254"

imdsApiVersion :: Text
imdsApiVersion = "2021-02-01"

{- | Provides a default @TokenCredential@ authentication flow for applications that will be deployed to Azure.

Currently only Managed Identity authentication is implemented.

TODO: Implement other auth flows such as @withAzureCli@ and @withEnvironment@ and then apply
      alternative instance to @defaultAzureCredential@
      It should be of the form:
      defaultAzureCredential =
            withManagedIdentity
        <|> withAzureCli
        <|> withEnvironment

      Order of authentication attempts:
        1. EnvironmentCredential
        2. Managed Identity (Only this is implemented at the moment)
        3. Azure CLI
-}
defaultAzureCredential ::
    MonadIO m =>
    -- | Client ID (optional, for user-assigned managed identity)
    Maybe Text ->
    -- | Azure Resource URI to get a token for
    ResourceUri ->
    -- | Token cache for storing and reusing tokens
    TokenCache ->
    m AccessToken
defaultAzureCredential = withManagedIdentity Nothing

{- | Fetches an Access token using Azure Managed Identity.

All errors are thrown in IO.

For a version where errors are returned in a @Left@ branch, use 'withManagedIdentityEither'.
-}
withManagedIdentity ::
    MonadIO m =>
    -- | Optional HTTP Manager (a new one is created if not provided)
    Maybe Manager ->
    -- | Client ID (optional, for user-assigned managed identity)
    Maybe Text ->
    -- | Resource URI
    ResourceUri ->
    -- | Token cache
    TokenCache ->
    m AccessToken
withManagedIdentity mgr clientId resourceUri tokenCache = do
    token <- withManagedIdentityEither mgr clientId resourceUri tokenCache
    case token of
        Left err -> throwIO err
        Right tok -> pure tok

{- | Fetches an Access token using Azure Managed Identity.

Returns errors in an @Either@ instead of throwing.
-}
withManagedIdentityEither ::
    MonadIO m =>
    -- | Optional HTTP Manager (a new one is created if not provided)
    Maybe Manager ->
    -- | Client ID (optional, for user-assigned managed identity)
    Maybe Text ->
    -- | Resource URI
    ResourceUri ->
    -- | Token cache
    TokenCache ->
    m (Either AccessTokenException AccessToken)
withManagedIdentityEither mManager clientId resourceUri tokenCache = do
    identityEndpoint <- lookupEnv "IDENTITY_ENDPOINT"
    identityHeader <- lookupEnv "IDENTITY_HEADER"
    case (,) <$> identityEndpoint <*> identityHeader of
        -- TODO: incorporate @IDENTITY_ENDPOINT@ into this logic
        --       If it's present, we can directly make a call to
        --       it and retrieve the access token.
        -- This functionality is only available on App Service and not standalone
        -- VM instances.
        Just (_endpoint, _header) ->
            pure . Left $ TokenEndpointNotAvailable "Fetching Access token on an App Service is not yet supported"
        -- We do not have the @IDENTITY_ENDPOINT@. Which means that
        -- the VM is possibly standalone and not inside an App Service.
        -- Therefore, in order to get the access token details, we need
        -- to make GET request to Azure Instance Metadata Service.
        -- But first, check for an existing token
        Nothing -> do
            tk <- readTokenCache tokenCache
            case tk of
                -- In case there is no existing token, we fetch a new one
                Nothing ->
                    fetchAndCacheToken mManager clientId resourceUri tokenCache (Text.pack <$> identityHeader)
                Just oldToken@AccessToken{atExpiresOn} -> do
                    -- we do have a token but we should check for its validity
                    isTokenExpired <- isExpired atExpiresOn
                    if isTokenExpired
                        then fetchAndCacheToken mManager clientId resourceUri tokenCache (Text.pack <$> identityHeader)
                        else pure $ Right oldToken

-- | Internal helper to fetch a new token and update the cache.
fetchAndCacheToken ::
    MonadIO m =>
    Maybe Manager ->
    Maybe Text ->
    ResourceUri ->
    TokenCache ->
    Maybe Text ->
    m (Either AccessTokenException AccessToken)
fetchAndCacheToken mManager clientId resourceUri tokenCache identityHeader = do
    newToken <- callAzureIMDSEndpoint mManager getAzureIMDSClient resourceUri clientId identityHeader
    case newToken of
        Left err -> pure . Left . TokenFetchFailed $ err
        Right tok -> do
            updateTokenCache tokenCache (Just tok)
            pure $ Right tok

-- | An exception that can occur when generating an 'AccessToken'.
data AccessTokenException
    = -- | The App Service token endpoint is not yet supported.
      TokenEndpointNotAvailable Text
    | -- | Failed to fetch token from IMDS.
      TokenFetchFailed Text
    deriving stock (Eq, Show, Typeable)

instance Exception AccessTokenException

type AzureIMDSEndpoint =
    "metadata"
        :> "identity"
        :> "oauth2"
        :> "token"
        :> QueryParam' '[Required, Strict] "api-version" Text
        :> QueryParam' '[Required, Strict] "resource" Text
        :> QueryParam' '[Optional, Strict] "client_id" Text
        :> Header' '[Optional, Strict] "x-identity-header" Text
        :> Header' '[Required, Strict] "Metadata" Bool
        :> Get '[JSON] AccessToken

getAzureIMDSClient ::
    Text ->
    Text ->
    Maybe Text ->
    Maybe Text ->
    Bool ->
    ClientM AccessToken
getAzureIMDSClient = client (Proxy @AzureIMDSEndpoint)

callAzureIMDSEndpoint ::
    MonadIO m =>
    Maybe Manager ->
    (Text -> Text -> Maybe Text -> Maybe Text -> Bool -> ClientM AccessToken) ->
    ResourceUri ->
    Maybe Text ->
    Maybe Text ->
    m (Either Text AccessToken)
callAzureIMDSEndpoint mManager action (ResourceUri resourceUri) clientId identityHeader = do
    manager <- maybe (liftIO $ newManager defaultManagerSettings) pure mManager
    res <-
        liftIO $
            runClientM
                (action imdsApiVersion resourceUri clientId identityHeader True)
                (mkClientEnv manager $ BaseUrl Http imdsHost 80 "")
    pure $ case res of
        Left err ->
            Left . Text.pack $ show err
        Right response ->
            Right response
