{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Azure.Types
    ( AzureEmailRequest (..)
    , AzureEmailResponse (..)

      -- * Types to work with email addresses.

    -- These types merely represent the address and
    -- are not responsible for validating them whatsoever.
    , EmailAddress (..)
    , EmailRecipients (..)
    , EmailContent (..)
    , EmailAttachment (..)
    ) where

import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, withText, (.:), (.=))
import Data.Aeson.Types (parseFail)
import Data.Text (Text)

{- | Each email is represented as an object with @displayName@
and an associated @address@.

Source: https://learn.microsoft.com/en-us/rest/api/communication/dataplane/email/send?view=rest-communication-dataplane-2023-03-31&tabs=HTTP#emailaddress
-}
data EmailAddress = EmailAddress
    { eaEmail :: !Text
    , eaDisplayName :: !Text
    }
    deriving stock (Eq, Show)

instance ToJSON EmailAddress where
    toJSON EmailAddress{..} =
        object
            [ "address" .= eaEmail
            , "displayName" .= eaDisplayName
            ]

-- | Fields to represent @cc@, @bcc@ and @to@ in an email
data EmailRecipients = EmailRecipients
    { ccRecipients :: ![EmailAddress]
    , bccRecipients :: ![EmailAddress]
    , toRecipients :: ![EmailAddress]
    }
    deriving stock (Show)

instance ToJSON EmailRecipients where
    toJSON EmailRecipients{..} =
        object
            [ "to" .= toRecipients
            , "cc" .= ccRecipients
            , "bcc" .= bccRecipients
            ]

-- | Azure email requires both HTML and plain text format email content
data EmailContent = EmailContent
    { ecHtml :: !Text
    -- ^ Html version of the email message.
    , ecPlainText :: !Text
    -- ^ Plain text version of the email message.
    , ecSubject :: !Text
    -- ^ Subject of the email message.
    }
    deriving stock (Eq, Show)

instance ToJSON EmailContent where
    toJSON EmailContent{..} =
        object
            [ "plainText" .= ecPlainText
            , "html" .= ecHtml
            , "subject" .= ecSubject
            ]

data EmailAttachment = EmailAttachment
    { eaContentInBase64 :: !Text
    -- ^ Base64 encoded contents of the attachment
    , eaContentType :: !Text
    -- ^ MIME type of the attachment
    , eaName :: !Text
    -- ^ Name of the attachment
    }
    deriving stock (Show)

instance ToJSON EmailAttachment where
    toJSON EmailAttachment{..} =
        object
            [ "name" .= eaName
            , "contentType" .= eaContentType
            , "contentInBase64" .= eaContentInBase64
            ]

{- | Represents the payload for sending an email message.

Source: https://learn.microsoft.com/en-us/rest/api/communication/dataplane/email/send?view=rest-communication-dataplane-2023-03-31&tabs=HTTP#emailmessage
-}
data AzureEmailRequest = AzureEmailRequest
    { aerContent :: !EmailContent
    , aerRecipients :: !EmailRecipients
    , aerSenderAddress :: !Text -- TODO: This should probably be it's own newtype
    , aerReplyTo :: ![EmailAddress] -- TODO: Should this be NonEmpty instead?
    , aerAttachments :: ![EmailAttachment]
    , aerUserEngagementTrackingDisabled :: !Bool
    }
    deriving stock (Show)

instance ToJSON AzureEmailRequest where
    toJSON AzureEmailRequest{..} =
        object
            [ "content" .= aerContent
            , "recipients" .= aerRecipients
            , "senderAddress" .= aerSenderAddress
            , "replyTo" .= aerReplyTo
            , "attachments" .= aerAttachments
            , "userEngagementTrackingDisabled" .= aerUserEngagementTrackingDisabled
            ]

{- | Possible states once a send email action is triggered.
Source: https://learn.microsoft.com/en-us/rest/api/communication/dataplane/email/send?view=rest-communication-dataplane-2023-03-31&tabs=HTTP#emailsendstatus
-}
data EmailSendStatus
    = Canceled
    | Failed
    | NotStarted
    | Running
    | Succeeded
    deriving stock (Eq, Show, Enum, Bounded)

instance FromJSON EmailSendStatus where
    parseJSON = withText "EmailSendStatus" $ \case
        "Canceled" -> pure Canceled
        "Failed" -> pure Failed
        "NotStarted" -> pure NotStarted
        "Running" -> pure Running
        "Succeeded" -> pure Succeeded
        invalidStatus -> parseFail $ "Unexpected EmailSendStatus: " <> show invalidStatus

data AzureEmailResponse = AzureEmailResponse
    { aerId :: !Text
    , aerStatus :: !EmailSendStatus
    }
    deriving stock (Eq, Show)

instance FromJSON AzureEmailResponse where
    parseJSON = withObject "AzureEmailResponse" $ \o -> do
        aerId <- o .: "id"
        aerStatus <- o .: "status"
        pure AzureEmailResponse{..}
