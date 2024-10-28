{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Azure.Blob.DeleteBlob
    ( deleteBlobObject
    , deleteBlobObjectEither
    , DeleteBlob (..)
    ) where

import Data.Data (Proxy (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.API
import Servant.Client (BaseUrl (..), ClientM, Scheme (..), client, mkClientEnv, runClientM)
import UnliftIO (MonadIO (..), throwString)

import Azure.Blob.Types (AccountName (..), BlobName (..), ContainerName (..))
import Azure.Blob.Utils (mkBlobHostUrl)
import Azure.Types (AccessToken (..))

import qualified Data.Text as Text

data DeleteBlob = DeleteBlob
    { accountName :: !AccountName
    , containerName :: !ContainerName
    , blobName :: !BlobName
    , accessToken :: !AccessToken
    }
    deriving stock (Eq, Generic)

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
callDeleteBlobClient action DeleteBlob{accountName, containerName, blobName, accessToken} = do
    manager <- liftIO newTlsManager
    res <-
        liftIO $
            runClientM
                (action containerName blobName ("Bearer " <> atAccessToken accessToken) "2020-04-08")
                (mkClientEnv manager $ BaseUrl Https (mkBlobHostUrl accountName) 443 "")
    pure $ case res of
        Left err -> do
            Left . Text.pack $ show err
        Right _ -> do
            pure ()
