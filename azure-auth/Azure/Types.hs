{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Azure.Types
    ( AccessToken (..)
    , TokenType (..)
    , ResourceUri (..)
    , TokenCache
    , newEmptyTokenCache
    , updateTokenCache
    , invalidateTokenCache
    , readTokenCache
    ) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, withText, (.:), (.=))
import Data.Text (Text)
import Data.Time.Clock.POSIX (POSIXTime)
import UnliftIO (MonadIO (..))
import UnliftIO.STM (TVar, atomically, newTVarIO, readTVarIO, writeTVar)

import qualified Data.Text as Text

-- | The type of access token. Currently only Bearer tokens are supported.
data TokenType
    = Bearer
    | OtherTokenType !Text
    deriving stock (Eq, Show)

instance FromJSON TokenType where
    parseJSON = withText "TokenType" $ \t ->
        pure $ case Text.toLower t of
            "bearer" -> Bearer
            other -> OtherTokenType other

instance ToJSON TokenType where
    toJSON Bearer = toJSON ("Bearer" :: Text)
    toJSON (OtherTokenType t) = toJSON t

{- | A newtype wrapper for Azure resource URIs.

Examples:

* @"https://vault.azure.net"@ for Key Vault
* @"https://storage.azure.com"@ for Blob Storage
* @"https://management.azure.com"@ for Azure Resource Manager
-}
newtype ResourceUri = ResourceUri {unResourceUri :: Text}
    deriving stock (Eq, Show)
    deriving newtype (FromJSON, ToJSON)

{- |
Data type representing a response body when GET request is made
using the Azure Instance Metadata Service (IMDS) endpoint.

Source: https://learn.microsoft.com/en-us/entra/identity/managed-identities-azure-resources/how-to-use-vm-token#get-a-token-using-http
-}
data AccessToken = AccessToken
    { atAccessToken :: !Text
    {- ^ The requested access token. When you call a secured REST API, the
    token is embedded in the Authorization request header field as a @bearer@
    token, allowing the API to authenticate the caller.
    -}
    , atExpiresIn :: !Int
    {- ^ The number of seconds the access token continues to be valid, before
    expiring, from time of issuance. Time of issuance can be found in
    the token's @iat@ claim.
    -}
    , atExpiresOn :: !POSIXTime
    {- ^ The timespan when the access token expires. The date is
    represented as the number of seconds from @1970-01-01T0:0:0Z UTC@
    (corresponds to the token's @exp@ claim).
    -}
    , atResource :: !ResourceUri
    {- ^ The resource the access token was requested for, which
    matches the resource query string parameter of the request.
    -}
    , atTokenType :: !TokenType
    {- ^ The type of token, which is a @Bearer@ access token, which means
    the resource can give access to the bearer of this token.
    -}
    }
    deriving stock (Eq, Show)

instance FromJSON AccessToken where
    parseJSON = withObject "AccessToken" $ \o -> do
        atAccessToken <- o .: "access_token"
        -- expires_in comes as a string from Azure IMDS
        expiresInStr <- o .: "expires_in"
        atExpiresIn <- case reads expiresInStr of
            [(n, "")] -> pure n
            _ -> fail $ "Could not parse expires_in: " <> expiresInStr
        -- expires_on comes as a string (unix timestamp) from Azure IMDS
        expiresOnStr <- o .: "expires_on"
        atExpiresOn <- case reads expiresOnStr of
            [(n, "")] -> pure (fromInteger n)
            _ -> fail $ "Could not parse expires_on: " <> expiresOnStr
        atResource <- o .: "resource"
        atTokenType <- o .: "token_type"
        pure AccessToken{..}

instance ToJSON AccessToken where
    toJSON AccessToken{..} =
        object
            [ "access_token" .= atAccessToken
            , "expires_in" .= show atExpiresIn
            , "expires_on" .= show (round atExpiresOn :: Integer)
            , "resource" .= atResource
            , "token_type" .= atTokenType
            ]

{- | Thread-safe mutable storage for caching an 'AccessToken'.

Use 'newEmptyTokenCache' to create, 'readTokenCache' to read,
'updateTokenCache' to update, and 'invalidateTokenCache' to force
a refresh on the next credential request.
-}
type TokenCache = TVar (Maybe AccessToken)

-- | Create an empty token cache.
newEmptyTokenCache :: MonadIO m => m TokenCache
newEmptyTokenCache = newTVarIO Nothing

{- | Invalidate the cached token, forcing a refresh on the next request.

Use this when you know a token has been revoked or when you need
to force re-authentication.
-}
invalidateTokenCache :: MonadIO m => TokenCache -> m ()
invalidateTokenCache cache = atomically $ writeTVar cache Nothing

-- | Update the token cache with a new value.
updateTokenCache :: MonadIO m => TokenCache -> Maybe AccessToken -> m ()
updateTokenCache cache accessToken = atomically $ writeTVar cache accessToken

-- | Read the current cached token, if any.
readTokenCache :: MonadIO m => TokenCache -> m (Maybe AccessToken)
readTokenCache = readTVarIO
