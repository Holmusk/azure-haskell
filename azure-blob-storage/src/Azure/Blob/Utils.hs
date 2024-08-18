module Azure.Blob.Utils
    ( blobStorageResourceUrl
    , mkBlobHostUrl
    ) where

import Azure.Blob.Types (AccountName (..))
import Data.Text (Text)

import qualified Data.Text as Text

blobStorageResourceUrl :: Text
blobStorageResourceUrl = "https://storage.azure.com/"

mkBlobHostUrl :: AccountName -> String
mkBlobHostUrl (AccountName accountName) = Text.unpack accountName <> ".blob.core.windows.net"
