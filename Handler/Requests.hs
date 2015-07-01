module Handler.Requests where

import Import
import Data.Time.LocalTime (LocalTime(..), utcToLocalTime, getTimeZone)
import Database.Persist.Sql(fromSqlKey)
import           Network.HTTP                (urlEncode)
import           Network.HTTP.Conduit
import           Data.Time.Calendar
import           Data.Time.Calendar.WeekDate (toWeekDate)
import           qualified Data.Text as T
import           Data.Text                   (split, unpack)
import qualified Data.List                   as List
import           Data.Aeson
import           Text.Read (read)
import Network.Mail.Mime
import Network.Mail.Mime.SES
import System.Random (newStdGen)
import qualified Data.ByteString.Lazy.UTF8 as LU
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)

-- | すべての休暇申請を取得するクエリ自体を、`widget`に含めているので再利用が簡単になる
-- allRequests :: Widget
-- allRequests = do
--     reqs <- handlerToWidget $ runDB $ do
--         selectList [] [Asc HolidayRequestCreatedAt]
--     $(widgetFile "requests")


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
getRequestsR :: Handler Html
getRequestsR = do
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
            createdAt <- liftIO getCurrentTime
            days <- liftIO $ theWeekday whenFrom whenTo
            case length days >= 1 of
                    True -> do
                        let daysDouble = fromIntegral $ length days :: Double
                        Entity bid _ <- runDB $ getBy404 $ UniqueHolidayBalance uid cid

                        -- すでに同じ日程で申請していたらエラー
                        reqestDetails <- runDB $ selectList [RequestDetailUser ==. uid] []
                        let requested = map (requestDetailDate . entityVal) reqestDetails
                        case length $ filter (`elem` requested) days of
                            0 -> do
                                rid <- runDB $ do
                                    rid <- insert $ HolidayRequest daysDouble whenFrom whenTo cid aid createdAt uid
                                    update bid [HolidayBalanceBalance -=. daysDouble]
                                    -- TODO:申請対象の日付の分だけ、明細レコードを挿入する
                                    forM_ days (\date -> do
                                        insert $ RequestDetail date "am" uid rid
                                        insert $ RequestDetail date "pm" uid rid
                                        return ()
                                        )
                                    return rid
                                rval <- runDB $ get404 rid
                                (myApprover $ Entity rid rval) >>= sendMailForApprove
                                setMessage $ toHtml $ "申請番号 " ++ show (fromSqlKey rid) ++ "を受け付けました"
                                redirect RequestsR
                            _ -> do
                                -- testSender
                                setMessage $ toHtml $ ("申請期間が重複しています" :: Text)
                                redirect RequestsR
                    _ -> do
                        setMessage $ toHtml ("申請期間に誤りがあります" :: Text)
                        redirect RequestsR
        _ -> do
            setMessage $ toHtml ("入力に誤りがあります" :: Text)
            redirect RequestsR


-- ==========================================================
-- | 将来的にはパッケージ化したいが、やり方がわからないのでこのファイルにベタ書きする
-- GNationalHoliday.hs

callJsonEndpoint :: (FromJSON j, Endpoint e) => e -> IO j
callJsonEndpoint e = do
    responseBody <- simpleHttp (buildURI e)
    case eitherDecode responseBody of
        Left err -> fail err
        Right res -> return res

toJSONDatetime :: Day -> String
toJSONDatetime d = showGregorian d ++ "T00:00:00Z"

-- このアプリのgoogle calendar key
appkey = "AIzaSyDiMjyHqqWbERHttkzsrvu1WemLIpHay4M"

-- | aはクエリパラメータを包んだデータ型であり、必要に応じでURIをそれらのパラメータで構築するための型クラス
--   a は、エンドポイントの数だけ実装することになりそう
class Endpoint a where
    buildURI :: a -> String

-- |
data GoogleCalendarEndpoint =
    GoogleCalendarEndpoint { timeMin :: !String, timeMax :: !String, key :: !String }

instance Endpoint GoogleCalendarEndpoint where
    buildURI GoogleCalendarEndpoint
        { timeMin = timeMin
        , timeMax = timeMax
        , key = key } =
            let params = [ ("timeMin", Just $ timeMin)
                         , ("timeMax", Just $ timeMax)
                         , ("key", Just $ key)
                         ]
            in "https://www.googleapis.com/calendar/v3/calendars/ja.japanese%23holiday@group.v.calendar.google.com/events"
                ++ renderQuery' True params

renderQuery' :: Bool -> [(String, Maybe String)] -> String
renderQuery' b params = (if b then "?" else "") ++ List.intercalate "&" serializedParams
  where serializedParams = catMaybes $ map renderParam params
        renderParam (key, Just val) = Just $ key ++ "=" ++ Network.HTTP.urlEncode val
        renderParam (_, Nothing) = Nothing

data NationalHoliday = NationalHoliday { date :: Day } deriving Show
newtype GoogleCalendarResponse =
        GoogleCalendarResponse { response :: [NationalHoliday] } deriving Show

-- | JSONをパースして、祝日リストを作る
instance FromJSON GoogleCalendarResponse where
    parseJSON (Object obj) =
        GoogleCalendarResponse <$> obj .: "items"
    parseJSON _ = mzero

instance FromJSON NationalHoliday where
    parseJSON (Object obj) = do
        (Object start) <- obj .: "start"
        (String date)  <- start .: "date"
        let [y, m, d] = split (== '-') date
        let holiday = fromGregorian (read (T.unpack y) :: Integer) (read (T.unpack m) :: Int) (read (T.unpack d) :: Int)
        return $ NationalHoliday holiday
    parseJSON _ = mzero

theNationalHolidays:: GoogleCalendarResponse -> IO [Day]
theNationalHolidays res = return $ map date $ response res

isWeekDay :: Day -> Bool
isWeekDay d
    | w == 6 = False
    | w == 7 = False
    | otherwise = True
  where
      (_, _, w) = toWeekDate d

-- | ある要素とリストをとって、その要素が含まれていなければ`True`を返す
contained :: Eq a => a -> [a] -> Bool
-- contained word = foldr (\x b -> (x == word) || b) False -- 論理和を取る形で畳み込む
contained word xs = word `notElem` xs

theWeekday :: Day -> Day -> IO [Day]
theWeekday s e = do
    gr <- callJsonEndpoint $ GoogleCalendarEndpoint (toJSONDatetime s) (toJSONDatetime e) appkey
    nh <- theNationalHolidays gr
    return $ filter (flip contained nh) $ filter isWeekDay [s..e]


-- ==========================================================
-- | 将来的にはパッケージ化したいが、やり方がわからないのでこのファイルにベタ書きする
-- SendEmail.hs

-- | 対象の申請者に対して、承認結果を知らせるメールを配信する
sendMailForUser :: Text -> Entity User -> Handler ()
sendMailForUser result user = do
    let email = (userIdent . entityVal) user
    let ses = SES
          { sesFrom = "rooooomania@gmail.com"
          , sesTo = [encodeUtf8 email]
          , sesAccessKey = "AKIAJAHK2HTHNJB2CIPA"
          , sesSecretKey = "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"
          , sesRegion = "us-west-2"
          }
    h <- getYesod
    render <- getUrlRender
    let rootr = render RootR
    renderSendMailSES (appHttpManager h) ses Mail
        { mailHeaders =
            [("Subject", "no reply")]
        , mailFrom = Address Nothing "rooooomania@gmail.com"
        , mailTo = [Address Nothing email]
        , mailCc = []
        , mailBcc = []
        , mailParts = return
            [ textPart
            , Part
               { partType = "text/html; charset=utf-8"
               , partEncoding = None
               , partFilename = Nothing
               , partContent = renderHtml [shamlet|\
    <p>あなたの休暇申請が #{result} されました。
    <p>次のリンクをクリックして、内容を確認してください。
    <p>
    <a href=#{rootr}>#{rootr}
    <p> よろしくお願いいたします。
    |]
               , partHeaders = []
               }
            ]
        }
      where
          textPart = Part "text/plain" None Nothing [] $ LU.fromString $ unlines
              [ "あなたの休暇申請が？？されました。"
              , "次のリンクをクリックして、内容を確認してください。"
            --   , T.unpack rootr
              ]

-- | 対象の承認者に対して、承認を促すメールを配信する
--   メールボディには、承認者向けの承認ビューへのリンクを記載する
sendMailForApprove :: Entity User -> Handler ()
sendMailForApprove approver = do
    let email = (userIdent . entityVal) approver
    let ses = SES
              { sesFrom = "rooooomania@gmail.com"
              , sesTo = [encodeUtf8 email]
              , sesAccessKey = "AKIAJAHK2HTHNJB2CIPA"
              , sesSecretKey = "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"
              , sesRegion = "us-west-2"
              }
    h <- getYesod
    render <- getUrlRender
    let rootr = render (ApproverR $ entityKey approver)
    renderSendMailSES (appHttpManager h) ses Mail
        { mailHeaders =
            [("Subject", "no reply")]
        , mailFrom = Address Nothing "rooooomania@gmail.com"
        , mailTo = [Address Nothing email]
        , mailCc = []
        , mailBcc = []
        , mailParts = return
            [ textPart
            , Part
               { partType = "text/html; charset=utf-8"
               , partEncoding = None
               , partFilename = Nothing
               , partContent = renderHtml [shamlet|\
<p>メンバーが休暇を申請しています。内容を確認してください。
<p>次のリンクをクリックして、承認者ページに進んでください。
<p>
   <a href=#{rootr}>#{rootr}
<p> よろしくお願いいたします。
|]
               , partHeaders = []
               }
            ]
        }
      where
          textPart = Part "text/plain" None Nothing [] $ LU.fromString $ unlines
              [ "メンバーが休暇を申請しています。内容を確認してください。"
              , "次のリンクをクリックして、承認者ページに進んでください。"
            --   , T.unpack rootr
              ]

-- | 休暇申請に紐づく、承認者データを返す
myApprover :: Entity HolidayRequest -> Handler (Entity User)
myApprover (Entity rid rval) = runDB $ do
    let uid = holidayRequestUser rval
    uval <- get404 $ uid
    let (Just approverId) = userApprover uval
    Just approver <- selectFirst [UserId ==. approverId] []
    return approver
