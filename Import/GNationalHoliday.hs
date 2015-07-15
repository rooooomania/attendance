{-# LANGUAGE OverloadedStrings #-}
module Import.GNationalHoliday where

import           Data.Aeson
import qualified Data.List                   as List
import           Data.Text                   (split, unpack)
import qualified Data.Text                   as T
import           Data.Time.Calendar
import           Data.Time.Calendar.WeekDate (toWeekDate)
import           Data.Time.LocalTime         (LocalTime (..), getTimeZone,
                                              utcToLocalTime)
import           Database.Persist.Sql        (fromSqlKey)
import           Import
import           Network.HTTP                (urlEncode)
import           Network.HTTP.Conduit
import           Text.Read                   (read)

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
