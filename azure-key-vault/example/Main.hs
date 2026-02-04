{-# LANGUAGE OverloadedStrings #-}

module Main where

import Azure.Auth (defaultAzureCredential, keyVaultResource, newEmptyTokenCache)
import Azure.Secret (getSecret)
import Azure.Secret.Types (KeyVaultHost (..), SecretName (..))

main :: IO ()
main = do
    cache <- newEmptyTokenCache
    cred <- defaultAzureCredential Nothing keyVaultResource cache
    -- In order to run this, you need to replace @SecretName@ and @KeyVaultHost@ with
    -- appropriate values in your resource group. These are just dummy values.
    getSecret (SecretName "radiohead") (KeyVaultHost "albums") cred >>= print
