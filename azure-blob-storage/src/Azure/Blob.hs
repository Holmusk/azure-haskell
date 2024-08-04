module Azure.Blob
    (
    -- ** Variants to fetch a blob object
      getBlobObject
    , getBlobObjectEither

    -- ** Variants to upload a blob to Blob storage
    , putBlobObject
    , putBlobObjectEither

    -- ** Variants for deleting a blob object
    , deleteBlobObject
    , deleteBlobObjectEither

    -- ** Generating a Shared Access Signature URI
    , generateSas

    -- ** Types for dealing with Blob storage functions
    , AccountName (..)
    , ContainerName (..)
    , BlobName (..)
    ) where

import Azure.Blob.PutBlob
import Azure.Blob.GetBlob
import Azure.Blob.DeleteBlob
import Azure.Blob.SharedAccessSignature
import Azure.Blob.Types
