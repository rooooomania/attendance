module Handler.Root where

import Import
import Data.Time.LocalTime (LocalTime(..), utcToLocalTime, getTimeZone)
import Data.Time.Calendar
import Database.Persist.Sql(fromSqlKey)

widgetUTCTime :: UTCTime -> Widget
widgetUTCTime utc = do
    zt <- liftIO $ getTimeZone utc
    let dt@(LocalTime d t) = utcToLocalTime zt utc
    [whamlet|
<td>#{show dt}
|]

data RequestForm = RequestForm
    { from :: Day
    , to   :: Day
    , category :: Entity Holiday
    }

widgetApproveStatus :: ApproveStatusId -> Widget
widgetApproveStatus aid = do
    val <- handlerToWidget $ runDB $ get404 aid
    let name = approveStatusName val
    [whamlet|
<td>#{name}
|]

widgetUserIdent :: UserId -> Widget
widgetUserIdent uid = do
    val <- handlerToWidget $ runDB $ get404 uid
    let ident = userIdent val
    [whamlet|
<td>#{ident}
|]

-- | ログインユーザのトップ画面。申請一覧と休暇申請フォーム、休暇残高を表示する
getRootR :: Handler Html
getRootR = do
    uid <- requireAuthId
    requests <- runDB $ selectList [HolidayRequestUser ==. uid] [Asc HolidayRequestWhenFrom] >>= mapM (\(Entity hid h) -> do
        category <- get404 $ holidayRequestCategory h
        status <- get404 $ holidayRequestStatus h
        return ((hid, h), holidayName category, approveStatusName status)
        )
    balances <- runDB $ selectList [HolidayBalanceUser ==. uid] [Asc HolidayBalanceId] >>= mapM (\(Entity bid b) -> do
        category <- get404 $ holidayBalanceCategory b
        return (b, holidayName category)
        )
    (form, enctype) <- generateFormPost requestHolidayForm
    defaultLayout $ do
        setTitle $ toHtml $ "ようこそ, " ++ show (fromSqlKey uid) ++ "さん。"
        $(widgetFile "root")


-- | 休暇申請フォーム
requestHolidayForm :: Form RequestForm
requestHolidayForm = renderDivs $ RequestForm
    <$> areq dayField "From" Nothing
    <*> areq dayField "To" Nothing
    <*> areq (selectField holidays) "休暇区分" Nothing
    where
        holidays = optionsPersist [] [Asc HolidayName] holidayName


-- | 特定のユーザに紐づく休暇申請を行う。残高も調整する
-- | implement registration for RequestDetail.
postRootR :: Handler Html
postRootR = do
    uid <- requireAuthId
    ((res, form), enctype) <- runFormPost requestHolidayForm
    case res of
        FormSuccess requestForm -> do
            let whenFrom = from requestForm
            let whenTo = to requestForm
            let Entity cid _ = category requestForm
            Entity aid _ <- runDB $ getBy404 $ UniqueApproveStatus "申請中"
            createdAt <- liftIO getCurrentTime
            let days = diffDays whenTo whenFrom + 1
            case days >= 1 of
                    True -> do
                        let daysDouble = fromInteger days :: Double
                        Entity bid _ <- runDB $ getBy404 $ UniqueHolidayBalance uid cid
                        rid <- runDB $ do
                            rid <- insert $ HolidayRequest daysDouble whenFrom whenTo cid aid createdAt uid
                            update bid [HolidayBalanceBalance -=. daysDouble]
                            return rid
                        setMessage $ toHtml $ "申請番号 " ++ show (fromSqlKey rid) ++ "を受け付けました"
                        redirect RootR
                    _ -> do
                        setMessage $ toHtml ("申請期間に誤りがあります" :: Text)
                        redirect RootR
        _ -> do
            setMessage $ toHtml ("入力に誤りがあります" :: Text)
            redirect RootR
