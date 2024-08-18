{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Azure.Blob.PutBlob
    ( putBlobObjectEither
    , putBlobObject
    , PutBlob (..)
    ) where

import Azure.Auth (defaultAzureCredential)
import Azure.Blob.Types (AccountName (..), BlobName (..), BlobType (..), ContainerName (..))
import Azure.Blob.Utils (blobStorageResourceUrl, mkBlobHostUrl)
import Data.ByteString (ByteString)
import Data.Data (Proxy (..))
import Data.Text (Text)
import GHC.Generics (Generic)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.API
import Servant.Client (BaseUrl (..), ClientM, Scheme (..), client, mkClientEnv, runClientM)
import UnliftIO (MonadIO (..), throwString)

import qualified Azure.Types as Auth
import qualified Data.Text as Text

{- | Adds a blob to a container.

You should have appropriate (Write) permissions in order to perform this operation.
-}
data PutBlob = PutBlob
    { accountName :: !AccountName
    , containerName :: !ContainerName
    , blobName :: !BlobName
    , tokenStore :: !Auth.Token
    , body :: !ByteString -- TODO: Add chunked upload
    }
    deriving stock (Eq, Generic)

{- | Upload a blob to a blob container.

Errors will be thrown in IO. For variant where error is
caught in a @Left@ branch, see @putBlobObjectEither@
-}
putBlobObject ::
    MonadIO m =>
    PutBlob ->
    m ()
putBlobObject putBlobReq = do
    res <- liftIO $ putBlobObjectEither putBlobReq
    case res of
        Left err ->
            throwString $ show err
        Right _ ->
            pure ()

-- | Upload a blob to a Blob container
putBlobObjectEither ::
    MonadIO m =>
    PutBlob ->
    m (Either Text ())
putBlobObjectEither putBlobreq = do
    res <-
        liftIO $
            callPutBlobClient putBlobObjectApi putBlobreq
    pure $
        case res of
            Right _ -> Right ()
            Left err -> Left err

-- | The following method works for all @BlobType@
type PutBlobApi =
    Capture "container-name" ContainerName
        :> Capture "blob-name" BlobName
        :> Header' '[Required, Strict] "Authorization" Text
        :> Header' '[Required, Strict] "x-ms-version" Text
        :> Header' '[Required, Strict] "x-ms-blob-type" Text
        :> ReqBody '[OctetStream] ByteString
        :> PutNoContent

putBlobObjectApi :: ContainerName -> BlobName -> Text -> Text -> Text -> ByteString -> ClientM NoContent
putBlobObjectApi = client (Proxy @PutBlobApi)

callPutBlobClient ::
    (ContainerName -> BlobName -> Text -> Text -> Text -> ByteString -> ClientM NoContent) ->
    PutBlob ->
    IO (Either Text ())
callPutBlobClient action PutBlob{accountName, containerName, blobName, tokenStore, body} = do
    Auth.AccessToken{atAccessToken} <- liftIO $ defaultAzureCredential Nothing blobStorageResourceUrl tokenStore
    manager <- liftIO newTlsManager
    res <-
        liftIO $
            runClientM
                (action containerName blobName ("Bearer " <> atAccessToken) "2020-04-08" (Text.pack $ show BlockBlob) body)
                (mkClientEnv manager $ BaseUrl Https (mkBlobHostUrl accountName) 443 "")
    pure $ case res of
        Left err ->
            Left . Text.pack $ show err
        Right _ ->
            Right ()
