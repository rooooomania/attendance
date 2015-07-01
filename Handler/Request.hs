module Handler.Request where

import           Import

data ApproveRequest = ApproveRequest
    {
      days      :: Double
    , from      :: Day
    , to        :: Day
    , category  :: Text
    , status    :: Entity ApproveStatus
    , createdAt :: Text
    , user      :: Text
    }

holidayRequestsForm :: (Maybe HolidayRequest, Maybe Holiday, Maybe User, Maybe (Entity ApproveStatus)) -> AForm Handler ApproveRequest
holidayRequestsForm (mhr, mh, mu, ma) = ApproveRequest
    <$> areq doubleField (bfs ("休暇期間" :: Text)) (holidayRequestDays <$> mhr)
    <*> areq dayField (bfs ("休暇開始" :: Text)) (holidayRequestWhenFrom <$> mhr)
    <*> areq dayField (bfs ("休暇終了" :: Text)) (holidayRequestWhenTo <$> mhr)
    <*> areq textField (bfs ("休暇区分" :: Text)) (holidayName <$> mh)
    <*> areq (selectField status) (bfs ("承認ステータス" :: Text)) ma
    <*> areq textField (bfs ("申請日" :: Text)) ((pack . show . holidayRequestCreatedAt) <$> mhr)
    <*> areq textField (bfs ("ユーザ" :: Text)) (userIdent <$> mu)
  where
    status = optionsPersist [] [Asc ApproveStatusId] approveStatusName

-- | 休暇申請に関連しているレコードも合わせて取得するためのヘルパ関数
findRequest :: Key HolidayRequest
               -> Handler (Maybe HolidayRequest, Maybe Holiday, Maybe User, Maybe (Entity ApproveStatus))
findRequest hid = runDB $ do
        --  休暇申請、ユーザ、承認ステータス、休暇区分のレコードを連結して`return`する
    h <- get404 hid
    ho <- get404 $ holidayRequestCategory h
    u <- get404 $ holidayRequestUser h
    a <- get404 $ holidayRequestStatus h
    sts <- getBy404 $ UniqueApproveStatus $ approveStatusName a -- 承認ステータスは、Entity で返す
    return (Just h, Just ho, Just u, Just sts)

-- | 個別の休暇申請を表示する。ここでは承認ステータスを変更できる
getRequestR :: HolidayRequestId -> Handler Html
getRequestR holidayRequestId = do
    req <- findRequest holidayRequestId
    (form, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ holidayRequestsForm req
    defaultLayout $ do
        [whamlet|
            <div .panel .panel-primary>
                <div .panel-heading>申請内容
                <div .panel-body>
                    <form role=form method=post enctype=#{enctype} action=@{RequestR holidayRequestId}?_method=PUT>
                        ^{form}
                        <button type=submit>更新
|]

putRequestR :: HolidayRequestId -> Handler Html
putRequestR holidayRequestId = do
    req <- findRequest holidayRequestId
    ((res, form), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ holidayRequestsForm req
    case res of
        FormSuccess ar -> do
            runDB $ do
                let sid = (entityKey . status) ar
                let sname = (approveStatusName . entityVal . status) ar
                Entity uid _ <- getBy404 $ UniqueUser $ user ar
                Entity cid _ <- getBy404 $ UniqueHoliday $ category ar
                Entity bid _ <- getBy404 $ UniqueHolidayBalance uid cid
                when (sname == "取消" || sname == "却下") (update bid [HolidayBalanceBalance +=. (days ar)])
                update holidayRequestId [HolidayRequestStatus =. sid]
                setMessage $ toHtml ("承認ステータスを更新しました" :: Text)
            redirect RequestsR -- rollback されないように、インデントに気をつける！
        _ -> do
            setMessage $ toHtml ("入力に誤りがあります" :: Text)
            defaultLayout $ do
                [whamlet|
                    <form role=form method=post enctype=#{enctype} action=@{RequestR holidayRequestId}?_method=PUT>
                        ^{form}
                        <button type=submit>更新
|]
