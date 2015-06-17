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


-- | すべての休暇申請を取得するクエリ自体を、`widget`に含めているので再利用が簡単になる
allRequests :: Widget
allRequests = do
    reqs <- handlerToWidget $ runDB $ do
        selectList [] [Asc HolidayRequestCreatedAt]
    $(widgetFile "requests")

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
-- | implement registration for RequestDetail.
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
                                setMessage $ toHtml $ "申請番号 " ++ show (fromSqlKey rid) ++ "を受け付けました"
                                redirect RequestsR
                            _ -> do
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
