{-# LANGUAGE DeriveGeneric #-}

module Azure.Blob.Types
    ( BlobName (..)
    , ContainerName (..)
    , AccountName (..)
    , PutBlob (..)
    , BlobType (..)
    , UserDelegationRequest (..)
    , UserDelegationResponse (..)
    , SasTokenExpiry (..)
    , Url (..)
    , SasPermissions (..)
    , sasPermissionsToText
    , SasResource (..)
    , sasResourceToText
    ) where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.ByteString (ByteString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API (ToHttpApiData)
import Xmlbf (FromXml (..), ToXml (..), element, pElement, pText, text)

import qualified Data.HashMap.Strict as HashMap

import qualified Azure.Types as Auth

newtype AccountName = AccountName
    { unAccountName :: Text
    }
    deriving stock (Eq, Show, Generic)
    deriving (ToHttpApiData) via Text

newtype ContainerName = ContainerName
    { unContainerName :: Text
    }
    deriving stock (Eq, Show, Generic)
    deriving (ToHttpApiData) via Text

newtype BlobName = BlobName
    { unBlobName :: Text
    }
    deriving stock (Eq, Show, Generic)
    deriving (ToHttpApiData) via Text

data BlobType
    = BlockBlob
    | PageBlob
    | AppendBlob
    deriving stock (Eq, Show, Generic)

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

{- | The fields are supposed to be ISO format strings
TODO: make these UTCTime formats
-}
data UserDelegationRequest = UserDelegationRequest
    { udrStartTime :: Text
    , udrExpiryTime :: Text
    }
    deriving stock (Eq, Show, Generic)

instance ToJSON UserDelegationRequest where
    toJSON UserDelegationRequest{..} =
        object
            [ "Start" .= udrStartTime
            , "Expiry" .= udrExpiryTime
            ]

instance ToXml UserDelegationRequest where
    toXml UserDelegationRequest{..} =
        element "KeyInfo" HashMap.empty $
            element "Start" HashMap.empty (text udrStartTime)
                <> element "Expiry" HashMap.empty (text udrExpiryTime)

data UserDelegationResponse = UserDelegationResponse
    { udrSignedKeyOid :: Text
    , udrSignedKeyStart :: Text
    , udrSignedKeyExpiry :: Text
    , udrSignedKeyService :: Text
    , udrSignedKeyVersion :: Text
    , udrSignedKeyTid :: Text
    -- ^ This the tenantID in which the service principle is defined
    , udrValue :: Text
    -- ^ User delegation key.
    -- Note that this cannot be used to grant access to blob resource directly.
    }
    deriving stock (Eq, Show, Generic)

instance FromXml UserDelegationResponse where
    fromXml = pElement "UserDelegationKey" $ do
        udrSignedKeyOid <- pElement "SignedOid" pText
        udrSignedKeyTid <- pElement "SignedTid" pText
        udrSignedKeyStart <- pElement "SignedStart" pText
        udrSignedKeyExpiry <- pElement "SignedExpiry" pText
        udrSignedKeyService <- pElement "SignedService" pText
        udrSignedKeyVersion <- pElement "SignedVersion" pText
        udrValue <- pElement "Value" pText
        pure UserDelegationResponse{..}

-- | Newtype for an url that can be fetched directly
newtype Url = Url
    { unUrl :: Text
    }
    deriving stock (Eq, Show, Generic)

-- | For an azure action to be turned into a signed url
newtype SasTokenExpiry = SasTokenExpiry
    { unSasTokenExpiry :: Int
    }

data SasPermissions
    = SasRead
    | SasAdd
    | SasCreate
    deriving stock (Eq, Show, Generic, Enum, Bounded)

{-# INLINE sasPermissionsToText #-}

-- | Reference: https://learn.microsoft.com/en-us/rest/api/storageservices/create-user-delegation-sas#specify-permissions
sasPermissionsToText :: SasPermissions -> Text
sasPermissionsToText = \case
    SasRead -> "r"
    SasAdd -> "a"
    SasCreate -> "c"

data SasResource
    = SasBlob
    | SasBlobVersion
    | SasBlobSnapshot
    | SasContainer
    | SasDirectory
    deriving stock (Eq, Show, Generic, Enum, Bounded)

{-# INLINE sasResourceToText #-}

-- | Reference: https://learn.microsoft.com/en-us/rest/api/storageservices/create-user-delegation-sas#specify-the-signed-resource-field
sasResourceToText :: SasResource -> Text
sasResourceToText = \case
    SasBlob -> "b"
    SasBlobVersion -> "bv"
    SasBlobSnapshot -> "bs"
    SasContainer -> "c"
    SasDirectory -> "d"
