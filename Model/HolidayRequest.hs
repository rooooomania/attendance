module Model.HolidayRequest where

import           Foundation
import           Import.NoFoundation

data RequestParams = RequestParams
    { paramFrom      :: Maybe Day
    , paramTo        :: Maybe Day
    , paramStatus    :: Maybe (Entity ApproveStatus)
    }

requestsWithParams :: RequestParams -> YesodDB App [Entity HolidayRequest]
requestsWithParams params = do
    requests <- selectList [] [Asc HolidayRequestId]
    return $ filter (applyfilter params . entityVal) requests

-- 休暇申請リストに適用して、問い合わせ対象のレコードを抽出する
-- True が戻れば、問い合わせ条件すべてに合致したことを意味する
applyfilter :: RequestParams -> HolidayRequest -> Bool
applyfilter params req = and
    [ go fromFilter paramFrom
    , go toFilter paramTo
    , go statusFilter paramStatus
    ]
  where
    -- パラメータの各値が Nothing なら True を返す
    -- Just ならその値を使ってレコード一つ一つを評価する
    go :: (a -> Bool) -> (RequestParams -> Maybe a) -> Bool
    go f g =
        case g params of
            Nothing -> True
            Just a  -> f a
    fromFilter   = (<= utctDay (holidayRequestCreatedAt req))
    toFilter     = (>= utctDay (holidayRequestCreatedAt req))
    statusFilter (Entity aid _) = (aid == holidayRequestStatus req)

-- 非同期処理で返すJson用のデータ
-- Modelに移行する予定
data AsyncRequests = AsyncRequests
    { requestDays :: Double
    , requestFrom :: Day
    , requestTo   :: Day
    , requestCategory :: Text
    , requestUser :: Text
    }

instance ToJSON AsyncRequests where
     toJSON AsyncRequests {..} = object
        [ "requestDays" .= requestDays
        , "requestFrom" .= show requestFrom
        , "requestTo"   .= show requestTo
        , "requestCategory" .= requestCategory
        , "requestUser" .= requestUser
        ]

-- モデルのデータ型を、JSON向けに編集するヘルパ関数

reqestUsedAPI :: HolidayRequest -> YesodDB App AsyncRequests
reqestUsedAPI req = do
    category <- get404 $ holidayRequestCategory req
    user     <- get404 $ holidayRequestUser req
    return AsyncRequests
            { requestDays = holidayRequestDays req
            , requestFrom = holidayRequestWhenFrom req
            , requestTo   = holidayRequestWhenTo req
            , requestCategory = holidayName category
            , requestUser = userIdent user
            }
