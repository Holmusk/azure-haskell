{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Azure.Auth
    ( defaultAzureCredential
    , withManagedIdentity
    , withManagedIdentityEither
    ) where

import Control.Exception (Exception)
import Data.Data (Proxy (..))
import Data.Text (Text)
import Data.Typeable (Typeable)
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Servant.API (Get, Header', JSON, Optional, QueryParam', Required, Strict, (:>))
import Servant.Client (BaseUrl (..), ClientM, Scheme (..), client, mkClientEnv, runClientM)
import UnliftIO (MonadIO (..), throwIO)
import UnliftIO.Environment (lookupEnv)

import Azure.Types (AccessToken (..), Token, readToken, updateToken)
import Azure.Utils (isExpired)

import qualified Data.Text as Text

{- | IMDS is a REST API that's available at a well-known, non-routable IP address ( 169.254. 169.254 ).
It is a local-only link can only be accessed from within the VM.
Communication between the VM and IMDS never leaves the host.
-}
imdsHost :: String
imdsHost = "169.254.169.254"

imdsApiVersion :: Text
imdsApiVersion = "2021-02-01"

{- | Provides a default @TokenCredential@ authentication flow for applications that will be deployed to Azure.

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
    -- | Client ID
    Maybe Text ->
    -- | Azure Resource URI (required for @managed identity@)
    Text ->
    -- | Token (if empty, then a new one is fetched and stored into the token TVar)
    Token ->
    m AccessToken
defaultAzureCredential = withManagedIdentity

{- | Fetches an Access token for autheticating different azure services
All errors are thrown in IO.

For version where errors are returned in a @Left@ branch, use @withManagedIdentityEither@
-}
withManagedIdentity ::
    MonadIO m =>
    -- | ClientId
    Maybe Text ->
    -- | Resource URI
    Text ->
    -- | Access Token
    Token ->
    m AccessToken
withManagedIdentity clientId resourceUri tokenStore = do
    token <- withManagedIdentityEither clientId resourceUri tokenStore
    case token of
        Left err -> throwIO err
        Right tok -> pure tok

withManagedIdentityEither ::
    MonadIO m =>
    -- | ClientId
    Maybe Text ->
    -- | Resource URI
    Text ->
    -- | Access Token
    Token ->
    m (Either AccessTokenException AccessToken)
withManagedIdentityEither clientId resourceUri tokenStore = do
    identityEndpoint <- lookupEnv "IDENTITY_ENDPOINT"
    identityHeader <- lookupEnv "IDENTITY_HEADER"
    case (,) <$> identityEndpoint <*> identityEndpoint of
        -- TODO: incorporate @IDENTITY_ENDPOINT@ into this logic
        --       If it's present, we can directly make a call to
        --       to it and retrieve the access token.
        -- This functionality is only available on App service and not standalone
        -- VM instances.
        Just (_endpoint, _header) ->
            pure . Left $ TokenEndpointNotAvailable "Fetching Access token on an app service is not yet supported"
        -- We do not have the @IDENTITY_ENDPOINT@. Which means that that
        -- the VM is possibly standalone and not inside an app service.
        -- Therefore, in order to get the access token details, we need
        -- to make GET request to Azure Instance Metadata Service.
        -- But first, check for an existing token
        Nothing -> do
            tk <- readToken tokenStore
            case tk of
                -- In case there is no existing token, we fetch a new one
                Nothing -> do
                    newToken <- callAzureIMDSEndpoint getAzureIMDSClient resourceUri clientId (Text.pack <$> identityHeader)
                    case newToken of
                        Left err -> pure . Left . TokenClientMismatch . Text.pack $ show err
                        Right tok -> do
                            updateToken tokenStore (Just tok)
                            pure $ Right tok
                Just oldToken@AccessToken{atExpiresOn} -> do
                    -- we do have a token but we should check for it's validity
                    isTokenExpired <- isExpired atExpiresOn
                    if isTokenExpired
                        then do
                            -- get a new token and write to the env
                            newToken <- callAzureIMDSEndpoint getAzureIMDSClient resourceUri clientId (Text.pack <$> identityHeader)
                            case newToken of
                                Left err -> pure . Left . TokenClientMismatch . Text.pack $ show err
                                Right tok -> do
                                    updateToken tokenStore (Just tok)
                                    pure $ Right tok
                        else pure $ Right oldToken

-- | An exception that can occur when generating an @AccessToken@
data AccessTokenException
    = TokenEndpointNotAvailable Text
    | TokenClientMismatch Text -- TODO: The type is misleading. This is a generic error from servant client
    deriving stock (Show, Typeable)

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
    (Text -> Text -> Maybe Text -> Maybe Text -> Bool -> ClientM AccessToken) ->
    Text ->
    Maybe Text ->
    Maybe Text ->
    m (Either Text AccessToken)
callAzureIMDSEndpoint action resourceUri clientId identityHeader = do
    manager <- liftIO $ newManager defaultManagerSettings
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
