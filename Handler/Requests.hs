module Handler.Requests where

import Import
import Data.Time.LocalTime (LocalTime(..), utcToLocalTime, getTimeZone)
import Database.Persist.Sql(fromSqlKey)
import           qualified Data.Text as T
import           Text.Read (read)

import Import.SendMail
import Import.GNationalHoliday

-- HolidayRequest を JSON 形式で返すために、ToJSON のインスタンスを与える
instance ToJSON HolidayRequest where
    toJSON HolidayRequest {..} = object
        [ "days" .= holidayRequestDays
        , "from" .= show holidayRequestWhenFrom
        , "to"   .= show holidayRequestWhenTo
        , "category" .= holidayRequestCategory
        , "status" .= holidayRequestStatus
        , "created_at" .= holidayRequestCreatedAt
        , "user" .= holidayRequestUser
        ]

-- | すべての休暇申請を取得するクエリ自体を、`widget`に含めているので再利用が簡単になる
allRequests :: Handler [HolidayRequest]
allRequests = do
    reqs <- runDB $ do
        selectList [] [Asc HolidayRequestId]
    return $ map entityVal reqs


widgetNoop = do
    [whamlet|
|]

requestsPerPage :: Maybe Text -> Widget
requestsPerPage mPageNumber = do
    let resultPerPage = 10
    let num = destPage mPageNumber
    reqs <- handlerToWidget $ runDB $ do
        selectList [] [ Asc HolidayRequestCreatedAt
                      , LimitTo resultPerPage
                      , OffsetBy $ (num - 1) * resultPerPage
                      ]
    $(widgetFile "requests")
  where
    destPage :: Maybe Text -> Int
    destPage mPage =
        case mPage of
            Just pageNumber  -> read $ T.unpack pageNumber :: Int
            _ -> 1 :: Int

data Direction = Prev | Next deriving Show

-- | 指定したページが開ける状態（レコードがある）なら、a要素をwidgetとして返す
getNextPage :: Int -> Direction -> Widget
getNextPage num _
    | num <= 0 = widgetNoop
getNextPage num direction = do
    let resultPerPage = 10
    mvalue <- handlerToWidget $
        runDB $ selectFirst []
            [ Asc HolidayRequestCreatedAt
            , OffsetBy $ (num - 1) * resultPerPage
            ]
    case mvalue of
        Just value -> [whamlet|
<li>
    <a href=@{RequestsR}?page=#{num}>#{show direction}
|]
        _ -> widgetNoop

-- ===============================================================

-- | `UTCTime`をDayに変換する。
-- IOアクションを伴うので、単純な内部関数としてではなく、`Widget`として取り扱う
widgetUTCTime :: UTCTime -> Widget
widgetUTCTime utc = do
    zt <- liftIO $ getTimeZone utc
    let dt@(LocalTime d t) = utcToLocalTime zt utc
    [whamlet|
<td>#{show dt}
|]

data RequestForm = RequestForm
    { user :: Entity User
    , from :: Day
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

-- | 二つの日付をとって、期間内の平日と、日数を返す
-- Googleカレンダーから祝日情報を取得する
--
getWeekdays :: Day -> Day -> Handler Html
getWeekdays = undefined

-- ===============================================================

-- | 休暇申請フォーム
requestHolidayForm :: AForm Handler RequestForm
requestHolidayForm = RequestForm
    <$> areq (selectField users) (bfs ("申請者" :: Text)) Nothing
    <*> areq dayField (bfs ("From" :: Text)) Nothing
    <*> areq dayField (bfs ("To" :: Text) )Nothing
    <*> areq (selectField holidays) (bfs ("休暇区分" :: Text)) Nothing
    where
        users = optionsPersist [] [Asc UserIdent] userIdent
        holidays = optionsPersist [] [Asc HolidayName] holidayName

-- | 休暇申請の一覧を表示する
getRequestsR :: Handler TypedContent
getRequestsR = do
    hs  <- allRequests
    hs' <- runDB $ mapM reqestUsedAPI hs
    selectRep $ do
        provideRep $ do
            -- リクエストパラメータから指定されたページ番号を取得する
            currentPage <- lookupGetParam "page"
            (form, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm requestHolidayForm
            defaultLayout $ do
                [whamlet|
<div .container>
    <div .row>
        <div .col-sm-4>
            <div .panel .panel-primary>
                <div .panel-heading>休暇申請
                <div .panel-body>
                    <form role=form method=post enctype=#{enctype}>
                        ^{form}
                        <button type=submit .btn .btn-primary>申請する
|]
        -- allRequests
                requestsPerPage currentPage
        provideJson hs'

-- | 特定のユーザに紐づく休暇申請を行う。残高も調整する
-- | implement registration for RequestDetail.
postRequestsR :: Handler Html
postRequestsR = do
    ((res, form), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm requestHolidayForm
    case res of
        FormSuccess requestForm -> do
            let Entity uid _ = user requestForm
            let whenFrom = from requestForm
            let whenTo = to requestForm
            let Entity cid _ = category requestForm
            Entity aid _ <- runDB $ getBy404 $ UniqueApproveStatus "申請中"
            -- ヘルパ関数を使って再利用
            requestHelper (uid, cid, whenFrom, whenTo, aid, RequestsR)
        _ -> do
            setMessage $ toHtml ("入力に誤りがあります" :: Text)
            redirect RequestsR

-- ヘルパ関数が受け取る引数が多いので、型シノニムを作る
type RequestInfo = (Key User, Key Holiday, Day, Day, Key ApproveStatus, Route App)
requestHelper :: RequestInfo -> Handler Html
requestHelper (uid, cid, from, to, aid, route) = do
    createdAt <- liftIO getCurrentTime
    days <- liftIO $ theWeekday from to

    -- 申請期間の入力が反対となっていたらエラー
    when (length days <= 0) $ do
        setMessage $ toHtml ("申請期間に誤りがあります" :: Text)
        redirect route

    let daysDouble = fromIntegral $ length days :: Double
    Entity bid b <- runDB $ getBy404 $ UniqueHolidayBalance uid cid

    -- 残高が不足していたらエラー
    when (holidayBalanceBalance b - daysDouble < 0) $ do
        setMessage $ toHtml $ ("そんなに休めませんよ" :: Text)
        redirect route

    -- すでに同じ日程で申請していたらエラー
    reqestDetails <- runDB $ selectList [RequestDetailUser ==. uid] []
    let requested = map (requestDetailDate . entityVal) reqestDetails
    when ((length $ filter (`elem` requested) days) > 0) $ do
        setMessage $ toHtml $ ("申請期間が重複しています" :: Text)
        redirect route

    rid <- runDB $ do
        rid <- insert $ HolidayRequest daysDouble from to cid aid createdAt uid
        update bid [HolidayBalanceBalance -=. daysDouble]
        createRequestDetail days uid rid
        return rid
    rval <- runDB $ get404 rid
    -- (myApprover $ Entity rid rval) >>= sendMailForApprove
    setMessage $ toHtml $ "申請番号 " ++ show (fromSqlKey rid) ++ "を受け付けました"
    redirect route
