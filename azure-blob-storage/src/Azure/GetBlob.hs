{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Azure.GetBlob
    ( getBlobObject
    , getBlobObjectEither
    ) where

import Azure.Auth (defaultAzureCredential)
import Azure.Blob.Types (AccountName (..), BlobName (..), ContainerName (..))
import Data.ByteString (ByteString, fromStrict, toStrict)
import Data.Data (Proxy (..))
import Data.List.NonEmpty (NonEmpty ((:|)))
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Client.TLS (newTlsManager)
import Network.HTTP.Media (MediaType)
import Servant.API
import Servant.Client (BaseUrl (..), ClientM, Scheme (..), client, mkClientEnv, runClientM)
import UnliftIO (MonadIO (..), throwString)

import qualified Azure.Types as Auth
import qualified Data.Text as Text
import qualified Network.HTTP.Media as M

blobStorageResourceUrl :: Text
blobStorageResourceUrl = "https://storage.azure.com/"

getBlobObject ::
    MonadIO m =>
    GetBlob ->
    m ByteString
getBlobObject getBlobReq = do
    res <- liftIO $ getBlobObjectEither getBlobReq
    case res of
        Left err ->
            throwString $ show err
        Right r ->
            pure r

getBlobObjectEither ::
    MonadIO m =>
    GetBlob ->
    m (Either Text ByteString)
getBlobObjectEither getBlobReq = do
    res <-
        liftIO $
            callGetBlobClient getBlobObjectApi getBlobReq
    pure $
        case res of
            Right r -> Right r
            Left err -> Left err

data GetBlob = GetBlob
    { accountName :: !AccountName
    , containerName :: !ContainerName
    , blobName :: !BlobName
    , tokenStore :: !Auth.Token
    }
    deriving stock (Eq, Generic)

-- | Phantom type to encapsulate the data type in servant client types
data Blob

type GetBlobApi =
    Capture "container-name" ContainerName
        :> Capture "blob-name" BlobName
        :> Header' '[Required, Strict] "Authorization" Text
        :> Header' '[Required, Strict] "x-ms-version" Text
        :> Get '[Blob] ByteString

instance Accept Blob where
    contentTypes :: Proxy Blob -> NonEmpty MediaType
    contentTypes _ =
        ("text" M.// "plain" M./: ("charset", "utf-8"))
            :| [ "application" M.// "octet-stream"
               , "text" M.// "csv"
               , "application" M.// "x-dbt"
               ]

instance MimeRender Blob ByteString where
    mimeRender _ = fromStrict

instance MimeUnrender Blob ByteString where
    mimeUnrender _ = Right . toStrict

getBlobObjectApi :: ContainerName -> BlobName -> Text -> Text -> ClientM ByteString
getBlobObjectApi = client (Proxy @GetBlobApi)

callGetBlobClient ::
    (ContainerName -> BlobName -> Text -> Text -> ClientM ByteString) ->
    GetBlob ->
    IO (Either Text ByteString)
callGetBlobClient action GetBlob{accountName, containerName, blobName, tokenStore} = do
    Auth.AccessToken{atAccessToken} <- liftIO $ defaultAzureCredential Nothing blobStorageResourceUrl tokenStore
    manager <- liftIO newTlsManager
    res <-
        liftIO $
            runClientM
                (action containerName blobName ("Bearer " <> atAccessToken) "2020-04-08")
                (mkClientEnv manager $ BaseUrl Https mkHostUrl 443 "")
    pure $ case res of
        Left err -> do
            Left . Text.pack $ show err
        Right response -> do
            Right response
  where
    mkHostUrl = Text.unpack (unAccountName accountName) <> ".blob.core.windows.net"
