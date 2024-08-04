{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Azure.Blob.DeleteBlob
    ( deleteBlobObject
    , deleteBlobObjectEither
    ) where

import Azure.Auth (defaultAzureCredential)
import Azure.Blob.Types (AccountName (..), BlobName (..), ContainerName (..))
import Data.Data (Proxy (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.API
import Servant.Client (BaseUrl (..), ClientM, Scheme (..), client, mkClientEnv, runClientM)
import UnliftIO (MonadIO (..), throwString)

import qualified Azure.Types as Auth
import qualified Data.Text as Text

blobStorageResourceUrl :: Text
blobStorageResourceUrl = "https://storage.azure.com/"

deleteBlobObject ::
    MonadIO m =>
    DeleteBlob ->
    m ()
deleteBlobObject getBlobReq = do
    res <- liftIO $ deleteBlobObjectEither getBlobReq
    case res of
        Left err ->
            throwString $ show err
        Right _ ->
            pure ()

deleteBlobObjectEither ::
    MonadIO m =>
    DeleteBlob ->
    m (Either Text ())
deleteBlobObjectEither getBlobReq = do
    res <-
        liftIO $
            callDeleteBlobClient deleteBlobObjectApi getBlobReq
    pure $
        case res of
            Right _ -> Right ()
            Left err -> Left err

data DeleteBlob = DeleteBlob
    { accountName :: !AccountName
    , containerName :: !ContainerName
    , blobName :: !BlobName
    , tokenStore :: !Auth.Token
    }
    deriving stock (Eq, Generic)

type DeleteBlobApi =
    Capture "container-name" ContainerName
        :> Capture "blob-name" BlobName
        :> Header' '[Required, Strict] "Authorization" Text
        :> Header' '[Required, Strict] "x-ms-version" Text
        :> DeleteNoContent

deleteBlobObjectApi :: ContainerName -> BlobName -> Text -> Text -> ClientM NoContent
deleteBlobObjectApi = client (Proxy @DeleteBlobApi)

callDeleteBlobClient ::
    (ContainerName -> BlobName -> Text -> Text -> ClientM NoContent) ->
    DeleteBlob ->
    IO (Either Text ())
callDeleteBlobClient action DeleteBlob{accountName, containerName, blobName, tokenStore} = do
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
        Right _ -> do
            pure ()
  where
    mkHostUrl = Text.unpack (unAccountName accountName) <> ".blob.core.windows.net"
