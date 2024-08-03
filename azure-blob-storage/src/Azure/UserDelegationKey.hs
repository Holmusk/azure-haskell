{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Azure.UserDelegationKey
    ( callGetUserDelegationKeyApi
    , getUserDelegationKeyApi
    ) where

import Azure.Auth (defaultAzureCredential)
import Azure.Blob.Types
    ( AccountName (..)
    , UserDelegationRequest (..)
    , UserDelegationResponse (..)
    )
import Data.Data (Proxy (..))
import Data.Text (Text)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.API
import Servant.Client (BaseUrl (..), ClientM, Scheme (..), client, mkClientEnv, runClientM)
import Servant.XML (XML)
import UnliftIO (MonadIO (..))

import qualified Azure.Types as Auth
import qualified Data.Text as Text

blobStorageResourceUrl :: Text
blobStorageResourceUrl = "https://storage.azure.com/"

-- These type aliases always hold static values.
-- Refer to azure docs: https://learn.microsoft.com/en-us/rest/api/storageservices/get-user-delegation-key#request
-- for the request URI syntax
type Comp = Text
type Restype = Text

-- Client for generating user delegation key.
-- This is used to generate SAS tokens for pre-signed URLs
-- in conjuntion with azure managed identity.
type GetUserDelegationKeyApi =
    QueryParam' '[Required, Strict] "restype" Restype
        :> QueryParam' '[Required, Strict] "comp" Comp
        :> Header' '[Required, Strict] "Authorization" Text
        :> Header' '[Required, Strict] "x-ms-version" Text
        :> ReqBody '[XML] UserDelegationRequest
        :> Post '[XML] UserDelegationResponse

getUserDelegationKeyApi :: Restype -> Comp -> Text -> Text -> UserDelegationRequest -> ClientM UserDelegationResponse
getUserDelegationKeyApi = client (Proxy @GetUserDelegationKeyApi)

callGetUserDelegationKeyApi ::
    (Restype -> Comp -> Text -> Text -> UserDelegationRequest -> ClientM UserDelegationResponse) ->
    AccountName ->
    Auth.Token ->
    UserDelegationRequest ->
    IO (Either Text UserDelegationResponse)
callGetUserDelegationKeyApi action accountName tokenStore req = do
    manager <- liftIO newTlsManager
    Auth.AccessToken{atAccessToken} <- liftIO $ defaultAzureCredential Nothing blobStorageResourceUrl tokenStore
    res <-
        liftIO $
            runClientM
                (action showResType showComp ("Bearer " <> atAccessToken) "2022-11-02" req)
                (mkClientEnv manager $ BaseUrl Https mkHostUrl 443 "")
    pure $ case res of
        -- TODO: this should actually log the error
        Left err ->
            Left . Text.pack $ show err
        Right resp ->
            Right resp
  where
    showComp = "userdelegationkey"
    showResType = "service"
    mkHostUrl = Text.unpack (unAccountName accountName) <> ".blob.core.windows.net"
