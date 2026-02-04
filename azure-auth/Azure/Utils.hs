module Azure.Utils
    ( isExpired
    ) where

import Data.Time.Clock.POSIX (POSIXTime, getPOSIXTime)
import UnliftIO (MonadIO (..))

{- | Check if an azure access token has expired.

Compares the expiration time against the current time.
-}
isExpired :: MonadIO m => POSIXTime -> m Bool
isExpired expiresOn = do
    currentTime <- liftIO getPOSIXTime
    pure $ expiresOn <= currentTime
