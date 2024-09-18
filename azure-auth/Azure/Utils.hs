module Azure.Utils where

import Data.Time (getCurrentTime, secondsToNominalDiffTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import UnliftIO (MonadIO (..))

import Azure.Types (ExpiresOn)

import qualified Data.Text as Text
import qualified Text.Read as Text

-- | Check if an azure access token has expired
isExpired :: MonadIO m => ExpiresOn -> m Bool
isExpired expiresOn = do
    let timestamp = posixSecondsToUTCTime . secondsToNominalDiffTime <$> Text.readMaybe (Text.unpack expiresOn)
    case timestamp of
        Just time -> do
            currentTime <- liftIO getCurrentTime
            return $ time <= currentTime
        Nothing ->
            return False
