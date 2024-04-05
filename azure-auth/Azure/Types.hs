module Azure.Types
    ( ExpiresOn
    , AccessToken (..)
    , Token
    , newEmptyToken
    , updateToken
    , expireToken
    , readToken
    ) where

import Data.Aeson (FromJSON (..), withObject, (.:))
import Data.Text (Text)
import UnliftIO (MonadIO (..), writeTVar)
import UnliftIO.STM (TVar, atomically, modifyTVar, newTVarIO, readTVarIO)

type ExpiresOn = Text

{- |
Data type representing a response body when GET request is made
using the Azure Instance Metadata Service (IMDS) endpoint.

Source: https://learn.microsoft.com/en-us/entra/identity/managed-identities-azure-resources/how-to-use-vm-token#get-a-token-using-http

TODO: Some of TokenType and Resource can possibly be represented using a sum type
      along with FromJSON instance.
-}
data AccessToken = AccessToken
    { atAccessToken :: !Text
    -- ^ The requested access token. When you call a secured REST API, the
    -- token is embedded in the Authorization request header field as a @bearer@
    -- token, allowing the API to authenticate the caller.
    , atRefreshToken :: !Text
    -- ^ Not used by managed identities for Azure resources.
    , atExpiresIn :: !Integer
    -- ^ The number of seconds the access token continues to be valid, before
    -- expiring, from time of issuance. Time of issuance can be found in
    -- the token's @iat@ claim.
    , atExpiresOn :: !ExpiresOn
    -- ^ The timespan when the access token expires. The date is
    -- represented as the number of seconds from @1970-01-01T0:0:0Z UTC@
    -- (corresponds to the token's @exp@ claim).
    -- NOTE: expires_on is a String version of unix epoch time, not an integer.
    , atResource :: !Text
    -- ^ The resource the access token was requested for, which
    -- matches the resource query string parameter of the request.
    , atTokenType :: !Text
    -- ^ The type of token, which is a @Bearer@ access token, which means
    -- the resource can give access to the bearer of this token.
    }
    deriving stock (Eq, Show)

instance FromJSON AccessToken where
    parseJSON = withObject "AccessToken" $ \o -> do
        atAccessToken <- o .: "access_token"
        atRefreshToken <- o .: "refresh_token"
        atExpiresIn <- o .: "expires_in"
        atExpiresOn <- o .: "expires_on"
        atResource <- o .: "resource"
        atTokenType <- o .: "token_type"
        pure AccessToken{..}

type Token = TVar (Maybe AccessToken)

newEmptyToken :: MonadIO m => m Token
newEmptyToken = newTVarIO Nothing

expireToken :: MonadIO m => Token -> m ()
expireToken token = atomically $ modifyTVar token (const Nothing)

updateToken :: MonadIO m => Token -> Maybe AccessToken -> m ()
updateToken tokenStore accessToken = atomically $ writeTVar tokenStore accessToken

-- | Read the current value of the token
readToken :: MonadIO m => Token -> m (Maybe AccessToken)
readToken = readTVarIO
