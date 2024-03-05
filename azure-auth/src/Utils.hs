module Utils where

import Data.Time (addUTCTime, getCurrentTime, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import Types (ExpiresOn)
import UnliftIO (MonadIO (..))

import qualified Text.Read as Text
import qualified Data.Text as Text

{- | Check if an azure access token expiration time
is past or < 20 seconds from current time
-}
isExpired :: MonadIO m => ExpiresOn -> m Bool
isExpired expiresOn = do
    let timestamp = posixSecondsToUTCTime . secondsToNominalDiffTime <$> Text.readMaybe (Text.unpack expiresOn)
    case timestamp of
        Just time -> do
            currentTime <- liftIO getCurrentTime
            return $ time <= addUTCTime 20 currentTime
        Nothing ->
            return False
