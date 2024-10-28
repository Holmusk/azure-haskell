{-# LANGUAGE OverloadedStrings #-}

module Main where

import Azure.Auth (defaultAzureCredential)
import Azure.Secret (getSecret)
import Azure.Secret.Types (KeyVaultHost (..), SecretName (..))
import Azure.Types (newEmptyToken)

main :: IO ()
main = do
    tok <- newEmptyToken
    cred <- defaultAzureCredential Nothing "https://vault.azure.net" tok
    -- In order to run this, you need to replace @SecretName@ and @KeyVaultHost@ with
    -- appropriate values in your resource group. These are just dummy values.
    getSecret (SecretName "radiohead") (KeyVaultHost "albums") cred >>= print
