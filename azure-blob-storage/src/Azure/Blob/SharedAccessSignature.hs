{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Azure.Blob.SharedAccessSignature
    ( generateSas
    , generateSasEither
    ) where

import Crypto.Hash.SHA256 (hmac)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Data.Time (UTCTime (..), addUTCTime, formatTime, getCurrentTime)
import Data.Time.Format (defaultTimeLocale)
import Network.HTTP.Types.URI (urlEncode)
import UnliftIO (MonadIO (..), throwString)

import Azure.Blob.Types
    ( AccountName (..)
    , BlobName (..)
    , ContainerName (..)
    , SasPermissions (..)
    , SasResource (..)
    , SasTokenExpiry (..)
    , Url (..)
    , UserDelegationRequest (..)
    , UserDelegationResponse (..)
    , sasPermissionsToText
    , sasResourceToText
    )
import Azure.Blob.UserDelegationKey (callGetUserDelegationKeyApi, getUserDelegationKeyApi)
import Azure.Types (AccessToken (..))

import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as Text

{- | Generates a Shared Access Token.

Errors will be thrown in IO. For variant where error is
caught in a @Left@ branch, see @generateSasEither@
-}
generateSas ::
    MonadIO m =>
    -- | Name of the Blob storage account
    AccountName ->
    -- | Name of the Blob container
    ContainerName ->
    -- | Name of the blob itself
    BlobName ->
    -- | Time in seconds for which the Shared Access Token should be valid for
    SasTokenExpiry ->
    -- | Access Token for making requests
    AccessToken ->
    m Url
generateSas accountName containerName blobName expiry accessToken = do
    eUrl <- liftIO $ generateSasEither accountName containerName blobName expiry accessToken
    case eUrl of
        Left err ->
            throwString $ show err
        Right url ->
            pure url

{- | Generates a Shared Access Token.

TODO: We need to add support for empty fields here. Eg: signedAuthorizedUserObjectId
-}
generateSasEither ::
    MonadIO m =>
    -- | Name of the Blob storage account
    AccountName ->
    -- | Name of the Blob container
    ContainerName ->
    -- | Name of the blob itself
    BlobName ->
    -- | Time in seconds for which the Shared Access Token should be valid for
    SasTokenExpiry ->
    -- | Access Token for making requests
    AccessToken ->
    m (Either Text Url)
generateSasEither accountName containerName blobName (SasTokenExpiry expiry) accessToken = do
    now <- liftIO getCurrentTime
    let isoStartTime = formatToAzureTime now
        isoExpiryTime = formatToAzureTime (addUTCTime (fromIntegral expiry) now)
    userDelgationKey <-
        liftIO $
            callGetUserDelegationKeyApi getUserDelegationKeyApi accountName accessToken (UserDelegationRequest isoStartTime isoExpiryTime)
    pure $ case userDelgationKey of
        Left err -> Left err
        Right UserDelegationResponse{..} -> do
            let canonicalizedResource =
                    "/blob/"
                        <> unAccountName accountName
                        <> "/"
                        <> unContainerName containerName
                        <> "/"
                        <> unBlobName blobName
                -- Source: https://learn.microsoft.com/en-us/rest/api/storageservices/create-user-delegation-sas#version-2020-12-06-and-later
                stringToSign =
                    sasPermissionsToText SasRead -- signedPermissions
                        <> "\n"
                        <> isoStartTime -- signedStart
                        <> "\n"
                        <> isoExpiryTime -- signedExpiry
                        <> "\n"
                        <> canonicalizedResource -- canonicalizedResource
                        <> "\n"
                        <> udrSignedKeyOid -- signedKeyObjectId
                        <> "\n"
                        <> udrSignedKeyTid -- signedKeyTenantId
                        <> "\n"
                        <> udrSignedKeyStart -- signedKeyStart
                        <> "\n"
                        <> udrSignedKeyExpiry -- signedKeyExpiry
                        <> "\n"
                        <> udrSignedKeyService -- signedKeyService
                        <> "\n"
                        <> udrSignedKeyVersion -- signedKeyVersion
                        <> "\n"
                        <> "" -- signedAuthorizedUserObjectId
                        <> "\n"
                        <> "" -- signedUnauthorizedUserObjectId
                        <> "\n"
                        <> "" -- signedCorrelationId
                        <> "\n"
                        <> "" -- signedIP
                        <> "\n"
                        <> "https" -- signedProtocol
                        <> "\n"
                        <> "2022-11-02" -- signedVersion
                        <> "\n"
                        <> sasResourceToText SasBlob -- signedResource
                        <> "\n"
                        <> "" -- signedSnapshotTime
                        <> "\n"
                        <> "" -- signedEncryptionScope
                        <> "\n"
                        <> "" -- rscc
                        <> "\n"
                        <> "" -- rscd
                        <> "\n"
                        <> "" -- rsce
                        <> "\n"
                        <> "" -- rscl
                        <> "\n"
                        <> "" -- rsct
            let sig = buildSignature stringToSign udrValue
            Right
                . Url
                $ "https://"
                    <> unAccountName accountName
                    <> ".blob.core.windows.net/"
                    <> unContainerName containerName
                    <> "/"
                    <> unBlobName blobName
                    <> "?sp="
                    <> sasPermissionsToText SasRead
                    <> "&st="
                    <> isoStartTime
                    <> "&se="
                    <> isoExpiryTime
                    <> "&skoid="
                    <> udrSignedKeyOid
                    <> "&sktid="
                    <> udrSignedKeyTid
                    <> "&skt="
                    <> udrSignedKeyStart
                    <> "&ske="
                    <> udrSignedKeyExpiry
                    <> "&sks="
                    <> udrSignedKeyService
                    <> "&skv="
                    <> udrSignedKeyVersion
                    <> "&sv=2022-11-02"
                    <> "&spr=https"
                    <> "&sr="
                    <> sasResourceToText SasBlob
                    <> "&sig="
                    <> decodeUtf8 (urlEncode True $ encodeUtf8 sig)
  where
    -- Date time formatting rules for azure:
    -- https://learn.microsoft.com/en-us/rest/api/storageservices/formatting-datetime-values
    formatToAzureTime :: UTCTime -> Text
    formatToAzureTime time = Text.pack $ formatTime defaultTimeLocale "%FT%TZ" time

    buildSignature :: Text -> Text -> Text
    buildSignature stringToSign secret =
        let decodedSecret = B64.decodeLenient (C8.pack (Text.unpack secret))
            encodedStringToSign = C8.pack (Text.unpack stringToSign)
            hashedBytes = hmac decodedSecret encodedStringToSign
            encodedSignature = B64.encode hashedBytes
         in decodeUtf8 encodedSignature
