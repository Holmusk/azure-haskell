{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Azure.Blob.GetBlob
    ( getBlobObject
    , getBlobObjectEither
    , GetBlob (..)
    ) where

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

import Azure.Blob.Types (AccountName (..), BlobName (..), ContainerName (..))
import Azure.Blob.Utils (mkBlobHostUrl)
import Azure.Types (AccessToken (..))

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as Text
import qualified Network.HTTP.Media as M

data GetBlob = GetBlob
    { accountName :: !AccountName
    , containerName :: !ContainerName
    , blobName :: !BlobName
    , accessToken :: !AccessToken
    }
    deriving stock (Eq, Generic)

getBlobObject ::
    MonadIO m =>
    GetBlob ->
    FilePath ->
    m ()
getBlobObject getBlobReq fp = do
    res <- liftIO $ getBlobObjectEither getBlobReq fp
    case res of
        Left err ->
            throwString $ show err
        Right r ->
            pure r

getBlobObjectEither ::
    MonadIO m =>
    GetBlob ->
    FilePath ->
    m (Either Text ())
getBlobObjectEither getBlobReq fp = do
    res <-
        liftIO $
            callGetBlobClient getBlobObjectApi getBlobReq
    case res of
        Right bs -> do
            liftIO $ LBS.writeFile fp (fromStrict bs)
            pure $ Right ()
        Left err -> pure $ Left err

-- | Phantom type to encapsulate the data type in servant client types
data Blob

type GetBlobApi =
    Capture "container-name" ContainerName
        :> Capture "blob-name" BlobName
        :> Header' '[Required, Strict] "Authorization" Text
        :> Header' '[Required, Strict] "x-ms-version" Text
        :> Get '[Blob] ByteString

-- TODO: this is more of a test at the moment.
-- GET endpoint should accept any blob that is available and not just
-- rely on certain mime types
instance Accept Blob where
    contentTypes :: Proxy Blob -> NonEmpty MediaType
    contentTypes _ =
        ("text" M.// "plain" M./: ("charset", "utf-8"))
            :| [ "application" M.// "octet-stream"
               , "text" M.// "csv"
               , "application" M.// "x-dbt"
               , "image" M.// "jpeg"
               , "image" M.// "png"
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
callGetBlobClient action GetBlob{accountName, containerName, blobName, accessToken} = do
    manager <- liftIO newTlsManager
    res <-
        liftIO $
            runClientM
                (action containerName blobName ("Bearer " <> atAccessToken accessToken) "2020-04-08")
                (mkClientEnv manager $ BaseUrl Https (mkBlobHostUrl accountName) 443 "")
    pure $ case res of
        Left err -> do
            Left . Text.pack $ show err
        Right response -> do
            Right response
