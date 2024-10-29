{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

module Azure.Email
    ( sendEmail
    , sendEmailEither
    ) where

import Azure.Types (AzureEmailRequest (..), AzureEmailResponse (..))
import Crypto.Hash.SHA256 (hash, hmac)
import Data.Aeson (encode)
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)
import Data.Time (UTCTime, defaultTimeLocale, formatTime, getCurrentTime)
import Network.HTTP.Client.TLS (newTlsManager)
import Servant.API
import Servant.Client (BaseUrl (..), ClientM, Scheme (..), client, mkClientEnv, runClientM)
import UnliftIO (MonadIO (..), throwString)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as Text

{- | Send an email provided a request payload

Errors are thrown in IO. For a variant where error is captured
in an @Left@ branch, see @sendEmailEither@
-}
sendEmail ::
    MonadIO m =>
    Text ->
    Text ->
    AzureEmailRequest ->
    m AzureEmailResponse
sendEmail apiSecret emailHost payload = do
    resp <- sendEmailEither apiSecret emailHost payload
    case resp of
        Left err -> throwString $ show err
        Right r -> pure r

-- | Send an email provided a request payload
sendEmailEither ::
    MonadIO m =>
    Text ->
    Text ->
    AzureEmailRequest ->
    m (Either Text AzureEmailResponse)
sendEmailEither apiSecret emailHost payload =
    liftIO $ callSendEmailClient sendEmailApi payload emailHost apiSecret

type SendEmailApi =
    "emails:send"
        :> QueryParam' '[Required, Strict] "api-version" Text
        :> Header' '[Required, Strict] "x-ms-date" Text
        :> Header' '[Required, Strict] "Host" Text
        :> Header' '[Required, Strict] "x-ms-content-sha256" Text
        :> Header' '[Required, Strict] "Authorization" Text
        :> ReqBody '[JSON] AzureEmailRequest
        :> Post '[JSON] AzureEmailResponse

sendEmailApi :: Text -> Text -> Text -> Text -> Text -> AzureEmailRequest -> ClientM AzureEmailResponse
sendEmailApi = client (Proxy @SendEmailApi)

callSendEmailClient ::
    (Text -> Text -> Text -> Text -> Text -> AzureEmailRequest -> ClientM AzureEmailResponse) ->
    AzureEmailRequest ->
    Text ->
    Text ->
    IO (Either Text AzureEmailResponse)
callSendEmailClient action req azureEmailHost secret = do
    manager <- liftIO newTlsManager
    (formatToAzureTime -> now) <- getCurrentTime
    encodedPayload <- encodePayload
    let stringToSign =
            "POST\n"
                <> "/emails:send?api-version="
                <> apiVersion
                <> "\n"
                <> now
                <> ";"
                <> azureEmailHost
                <> ";"
                <> encodedPayload
    let sign = buildSignature stringToSign secret
    res <-
        liftIO $
            runClientM
                (action apiVersion now azureEmailHost encodedPayload ("HMAC-SHA256 SignedHeaders=x-ms-date;host;x-ms-content-sha256&Signature=" <> sign) req)
                (mkClientEnv manager $ BaseUrl Https (Text.unpack azureEmailHost) 443 "")
    pure $ case res of
        Left err -> do
            Left . Text.pack $ show err
        Right r -> do
            Right r
  where
    apiVersion :: Text
    apiVersion = "2023-03-31"

    encodePayload :: IO Text
    encodePayload = do
        let contentBytes = encode req
            hashedBytes = hash (BS.toStrict contentBytes)
            encodedHash = B64.encode hashedBytes
        pure $ decodeUtf8 encodedHash

    -- TODO: formatToAzureTime and buildSignature are borrowed from azure-blob-storage.
    -- We should not be duplicating these utility functions
    formatToAzureTime :: UTCTime -> Text
    formatToAzureTime time = Text.pack $ formatTime defaultTimeLocale "%FT%TZ" time

    buildSignature :: Text -> Text -> Text
    buildSignature stringToSign sec =
        let decodedSecret = B64.decodeLenient (C8.pack (Text.unpack sec))
            encodedStringToSign = C8.pack (Text.unpack stringToSign)
            hashedBytes = hmac decodedSecret encodedStringToSign
            encodedSignature = B64.encode hashedBytes
         in decodeUtf8 encodedSignature
