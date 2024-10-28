{-# LANGUAGE OverloadedStrings #-}

module Main where

import System.Directory (doesFileExist)

import Azure.Auth (defaultAzureCredential)
import Azure.Blob.GetBlob (GetBlob (..), getBlobObject)
import Azure.Blob.Types (AccountName (..), BlobName (..), ContainerName (..))
import Azure.Types (newEmptyToken)

main :: IO ()
main = do
    tok <- newEmptyToken
    cred <- defaultAzureCredential Nothing "https://storage.azure.com" tok
    -- In order to run this, you need to replace @AccountName@, @ContainerName@ and @BlobName@
    -- with appropriate values in your resource group. These are just dummy values.
    let account = AccountName "OneRepublic"
        container = ContainerName "Native"
        blob = BlobName "counting_stars.jpeg"
        getBlobPayload = GetBlob account container blob cred
    getBlobObject getBlobPayload "/tmp/counting_stars.jpeg"
    doesFileExist "/tmp/counting_stars.jpeg" >>= print
