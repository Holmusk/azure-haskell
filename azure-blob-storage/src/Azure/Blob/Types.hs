{-# LANGUAGE DeriveGeneric #-}

module Azure.Blob.Types
    ( BlobName (..)
    , ContainerName (..)
    , AccountName (..)
    , PutBlob (..)
    , BlobType (..)
    ) where

import Data.ByteString (ByteString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API (ToHttpApiData)

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
