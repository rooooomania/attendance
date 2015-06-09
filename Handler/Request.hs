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

holidayRequestsForm :: (Maybe HolidayRequest, Maybe Holiday, Maybe User, Maybe (Entity ApproveStatus)) -> Form ApproveRequest
holidayRequestsForm (mhr, mh, mu, ma) = renderDivs $ ApproveRequest
    <$> areq doubleField "休暇期間" (holidayRequestDays <$> mhr)
    <*> areq dayField "休暇開始" (holidayRequestWhenFrom <$> mhr)
    <*> areq dayField "休暇終了" (holidayRequestWhenTo <$> mhr)
    <*> areq textField "休暇区分" (holidayName <$> mh)
    <*> areq (selectField status) "承認ステータス" ma
    <*> areq textField "申請日" ((pack . show . holidayRequestCreatedAt) <$> mhr)
    <*> areq textField "ユーザ" (userIdent <$> mu)
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
    (form, enctype) <- generateFormPost $ holidayRequestsForm req
    defaultLayout $ do
        [whamlet|
            <form method=post enctype=#{enctype} action=@{RequestR holidayRequestId}?_method=PUT>
                ^{form}
                <button type=submit>更新
|]

putRequestR :: HolidayRequestId -> Handler Html
putRequestR holidayRequestId = do
    req <- findRequest holidayRequestId
    ((res, form), enctype) <- runFormPost $ holidayRequestsForm req
    case res of
        FormSuccess ar -> do
            runDB $ do
                let sid = (entityKey . status) ar
                update holidayRequestId [HolidayRequestStatus =. sid]
                setMessage $ toHtml ("承認ステータスを更新しました" :: Text)
            redirect RequestsR -- rollback されないように、インデントに気をつける！
        _ -> do
            setMessage $ toHtml ("入力に誤りがあります" :: Text)
            defaultLayout $ do
                [whamlet|
                    <form method=post enctype=#{enctype} action=@{RequestR holidayRequestId}?_method=PUT>
                        ^{form}
                        <button type=submit>更新
|]
