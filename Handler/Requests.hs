module Handler.Requests where

import Import
import Data.Time.LocalTime (LocalTime(..), utcToLocalTime, getTimeZone)
import Data.Time.Calendar


-- | すべての休暇申請を取得するクエリ自体を、`widget`に含めているので再利用が簡単になる
allRequests :: Widget
allRequests = do
    reqs <- handlerToWidget $ runDB $ do
        rs <- selectList [] [Asc HolidayRequestCreatedAt]
        return $ map entityVal rs
    $(widgetFile "requests")

-- | `UTCTime`をDayに変換する。
-- IOアクションを伴うので、単純な内部関数としてではなく、`Widget`として取り扱う
convertToDay :: UTCTime -> Widget
convertToDay utc = do
    zt <- liftIO $ getTimeZone utc
    let (LocalTime d t) = utcToLocalTime zt utc
    [whamlet|
<td>#{show d}
|]

data RequestForm = RequestForm
    { user :: Entity User
    , from :: Day
    , to   :: Day
    , category :: Entity Holiday
    }

-- | 休暇申請フォーム
requestHolidayForm :: Form RequestForm
requestHolidayForm = renderDivs $ RequestForm
    <$> areq (selectField users) "申請者" Nothing
    <*> areq dayField "From" Nothing
    <*> areq dayField "To" Nothing
    <*> areq (selectField holidays) "休暇区分" Nothing
    where
        users = optionsPersist [] [Asc UserIdent] userIdent
        holidays = optionsPersist [] [Asc HolidayName] holidayName

-- | 休暇申請の一覧を表示する
getRequestsR :: Handler Html
getRequestsR = do
    (form, enctype) <- generateFormPost requestHolidayForm
    defaultLayout $ do
        [whamlet|
<form method=post enctype=#{enctype}>
    ^{form}
    <button type=submit>申請する
|]
        allRequests

-- | 特定のユーザに紐づく休暇申請を行う。残高も調整する
postRequestsR :: Handler Html
postRequestsR = do
    ((res, form), enctype) <- runFormPost requestHolidayForm
    case res of
        FormSuccess requestForm -> do
            let Entity uid _ = user requestForm
            let whenFrom = from requestForm
            let whenTo = to requestForm
            let Entity cid _ = category requestForm
            Entity aid _ <- runDB $ getBy404 $ UniqueApproveStatus "申請中"
            createdAt <- liftIO getCurrentTime
            let days = diffDays whenTo whenFrom
            let daysDouble = fromInteger days :: Double
            Entity bid _ <- runDB $ getBy404 $ UniqueHolidayBalance uid cid

            rid <- runDB $ do
                rid <- insert $ HolidayRequest daysDouble whenFrom whenTo cid aid createdAt uid
                update bid [HolidayBalanceBalance -=. daysDouble]
                return rid

            setMessage $ toHtml $ "申請番号 " ++ show rid ++ "を受け付けました"
            redirect RequestsR
        _ -> do
            setMessage $ toHtml ("入力に誤りがあります" :: Text)
            redirect RequestsR
