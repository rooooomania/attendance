module Handler.Approver where

import Import
import Data.Time.LocalTime (LocalTime(..), utcToLocalTime, getTimeZone)
import Data.Time.Calendar
import Database.Persist.Sql(toSqlKey, fromSqlKey)
import Handler.Requests(widgetUTCTime)
import Handler.Common(IsActiveTab(..), showPortalTab)
import Import.SendMail

-- | メール内のURIを踏んで、GETメソッドが呼び出される
--   自分が確認すべき休暇申請のリストと、申請または却下ボタンが表示される
getApproverR :: UserId -> Handler Html
getApproverR userId = do
    -- 休暇申請のリスト
    -- 被承認ユーザの申請リストを検索する。型は m [([Entity HolidayRequest], Text)]
    allRequests <- runDB $
        selectList [UserApprover ==. Just userId] [] >>= mapM (\(Entity uid uval) -> do
            requests <- selectList [HolidayRequestUser ==. uid, HolidayRequestStatus ==. toSqlKey 1] []
            let ident = userIdent uval
            return (requests, ident)
            )
    defaultLayout $ do
        showPortalTab Approver
        $(widgetFile "approver")

putApproverR :: UserId -> Handler Html
putApproverR userId = error "Not yet implemented: putApproverR"

-- | 承認ステータスを「承認」へ更新する
putApproveR :: UserId -> HolidayRequestId -> Handler ()
putApproveR uid hid = do
    user <- runDB $ do
        update hid [HolidayRequestStatus =. toSqlKey 2]
        h <- get404 hid
        u <- get404 $ holidayRequestUser h
        setMessage $ toHtml $ "申請番号" ++ show (fromSqlKey hid) ++ "を承認しました"
        return $ Entity (holidayRequestUser h) u
    sendMailForUser "承認" user
    redirect $ ApproverR uid

putRejectR :: UserId -> HolidayRequestId -> Handler ()
putRejectR uid hid = do
    user <- runDB $ do
        update hid [HolidayRequestStatus =. toSqlKey 3]
        h <- get404 hid
        u <- get404 $ holidayRequestUser h
        Entity bid _ <- getBy404 $ UniqueHolidayBalance (holidayRequestUser h) (holidayRequestCategory h)
        update bid [HolidayBalanceBalance +=. holidayRequestDays h]
        deleteWhere [RequestDetailDate >=.  holidayRequestWhenFrom h, RequestDetailDate <=. holidayRequestWhenTo h]
        setMessage $ toHtml $ "申請番号" ++ show (fromSqlKey hid) ++ "を却下しました"
        return $ Entity (holidayRequestUser h) u
    sendMailForUser "却下" user
    redirect $ ApproverR uid
