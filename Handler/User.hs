module Handler.User where

import Import

-- | 各ユーザの情報を表示する。編集可能なフィールドは、`textField`を用いる。
userForm :: Maybe User -> AForm Handler User
userForm muser = User
    <$> areq uniqueEmailField (bfs ("email" :: Text)) {fsAttrs = [("readonly", "")]} (userIdent <$> muser)
    <*> aopt passwordField (bfs ("パスワード" :: Text)) (userPassword <$> muser)
    <*> aopt textField (bfs ("氏名" :: Text)) (userFullName <$> muser)
    <*> aopt textField (bfs ("ニックネーム" :: Text)) (userNicname <$> muser)
    <*> aopt hiddenField (bfs ("承認者" :: Text)) (userApprover <$> muser)
    <*> areq boolField (bfs ("管理者権限" :: Text)) (userAdmin <$> muser)
    where
        -- 登録アドレスの重複チェックを行うよう、`custom field` をつくる
        uniqueEmailField = checkM isDuplicated emailField
        isDuplicated email = runDB $ do
            muser <- getBy $ UniqueUser email
            case muser of
                Just _ -> return $ Left ("すでに登録済みのユーザです" :: Text)
                Nothing -> return $ Right email

userFormUpdate :: Maybe User -> AForm Handler User
userFormUpdate muser = User
    <$> areq emailField (bfs ("email" :: Text)) {fsAttrs = [("readonly", "")]} (userIdent <$> muser)
    <*> aopt passwordField (bfs ("パスワード" :: Text)) (userPassword <$> muser)
    <*> aopt textField (bfs ("氏名" :: Text)) (userFullName <$> muser)
    <*> aopt textField (bfs ("ニックネーム" :: Text)) (userNicname <$> muser)
    <*> aopt hiddenField (bfs ("承認者" :: Text)) (userApprover <$> muser)
    <*> areq boolField (bfs ("管理者権限" :: Text)) (userAdmin <$> muser)


getUserR :: UserId -> Handler Html
getUserR userId = do
    muser <- runDB $ get userId
    (form, enctype) <- generateFormPost $ renderBootstrap3 BootstrapBasicForm $ userFormUpdate muser
    defaultLayout $ do
        $(widgetFile "user")

-- | フォームに記載された情報で、データベースのレコードを更新する。
putUserR :: UserId -> Handler Html
putUserR userId = do
    user <- runDB $ get404 userId
    ((res,form), enctype) <- runFormPost $ renderBootstrap3 BootstrapBasicForm $ userFormUpdate $ Just user
    case res of
        FormSuccess userUpdate -> do
            runDB $ update userId [ UserPassword =. userPassword userUpdate, UserFullName =. userFullName userUpdate , UserNicname  =. userNicname  userUpdate, UserApprover =. userApprover userUpdate, UserAdmin =. userAdmin userUpdate ]
            setMessage $ toHtml ("内容を更新しました" :: Text)
            redirect $ UserR userId
        _ -> do
            setMessage $ toHtml ("入力に誤りがあります" :: Text)
            defaultLayout $ [whamlet|
<form method=post enctype=#{enctype} action=@{UserR userId}?_method=PUT>
    ^{form}
    <button type=submit>編集
|]

putCancelR :: UserId -> HolidayRequestId -> Handler ()
putCancelR userId requestId = do
    runDB $ do
        Entity aid _ <- getBy404 $ UniqueApproveStatus "取消"
        h <- get404 requestId
        Entity bid _ <- getBy404 $ UniqueHolidayBalance userId (holidayRequestCategory h)
        update requestId [HolidayRequestStatus =. aid]
        update bid [HolidayBalanceBalance +=. (holidayRequestDays h)]
        deleteWhere [RequestDetailDate >=.  holidayRequestWhenFrom h, RequestDetailDate <=. holidayRequestWhenTo h]
    redirect RootR
