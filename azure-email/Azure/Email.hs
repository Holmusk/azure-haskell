{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}

module Azure.Email () where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Text (Text)
import GHC.Generics (Generic)

data EmailAddress = EmailAddress
    { eaEmail :: !Text
    , eaDisplayName :: !Text
    }
    deriving stock (Eq, Show, Generic)

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
    deriving stock (Generic)

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
    deriving stock (Eq, Show, Generic)

instance ToJSON EmailContent where
    toJSON EmailContent{..} =
        object
            [ "plainText" .= ecPlainText
            , "html" .= ecHtml
            , "subject" .= ecSubject
            ]

{- | Source:
https://learn.microsoft.com/en-us/rest/api/communication/dataplane/email/send?view=rest-communication-dataplane-2023-03-31&tabs=HTTP
-}
data AzureEmailRequest = AzureEmailRequest
    { aerContent :: !EmailContent
    , aerRecipients :: !EmailRecipients
    , aerSenderAddress :: !Text -- TODO: This should probably be it's own newtype
    }
    deriving stock (Generic)

instance ToJSON AzureEmailRequest where
    toJSON AzureEmailRequest{..} =
        object
            [ "content" .= aerContent
            , "recipients" .= aerRecipients
            , "senderAddress" .= aerSenderAddress
            ]
