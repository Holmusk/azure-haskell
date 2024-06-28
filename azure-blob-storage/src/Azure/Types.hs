{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}

-- Introduce blob storage functions that run in IO
module Azure.Types
    ( BlobName (..)
    , ContainerName (..)
    , AccountName (..)
    ) where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import GHC.Generics (Generic)
import Servant.API (ToHttpApiData)

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
