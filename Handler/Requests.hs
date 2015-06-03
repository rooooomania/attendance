module Handler.Requests where

import Import
import Data.Time.LocalTime (LocalTime(..), utcToLocalTime, getTimeZone)

-- | 休暇申請の一覧を表示する
getRequestsR :: Handler Html
getRequestsR = do
    reqs <- runDB $ do
        rs <- selectList [] [Asc HolidayRequestCreatedAt]
        return $ map entityVal rs

    defaultLayout $(widgetFile "requests")

-- | `UTCTime`をDayに変換する
convertToDay :: UTCTime -> Widget
convertToDay utc = do
    zt <- liftIO $ getTimeZone utc
    let (LocalTime d t) = utcToLocalTime zt utc
    [whamlet|
<td>#{show d}
|]


-- | 特定のユーザに紐づく休暇申請を行う
postRequestsR :: Handler Html
postRequestsR = error "Not yet implemented: postRequestsR"
