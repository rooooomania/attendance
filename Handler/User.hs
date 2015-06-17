module Handler.User where

import Import

-- | 各ユーザの情報を表示する。編集可能なフィールドは、`textField`を用いる。
userForm :: Maybe User -> Form User
userForm muser = renderDivs $ User
    <$> areq uniqueEmailField "email" {fsAttrs = [("readonly", "")]} (userIdent <$> muser)
    <*> aopt passwordField "パスワード" (userPassword <$> muser)
    <*> aopt textField "氏名" (userFullName <$> muser)
    <*> aopt textField "ニックネーム" (userNicname <$> muser)
    <*> aopt hiddenField "承認者" (userApprover <$> muser)
    <*> areq boolField "管理者権限" (userAdmin <$> muser)
    where
        -- 登録アドレスの重複チェックを行うよう、`custom field` をつくる
        uniqueEmailField = checkM isDuplicated emailField
        isDuplicated email = runDB $ do
            muser <- getBy $ UniqueUser email
            case muser of
                Just _ -> return $ Left ("すでに登録済みのユーザです" :: Text)
                Nothing -> return $ Right email

userFormUpdate :: Maybe User -> Form User
userFormUpdate muser = renderDivs $ User
    <$> areq emailField "email" {fsAttrs = [("readonly", "")]} (userIdent <$> muser)
    <*> aopt passwordField "パスワード" (userPassword <$> muser)
    <*> aopt textField "氏名" (userFullName <$> muser)
    <*> aopt textField "ニックネーム" (userNicname <$> muser)
    <*> aopt hiddenField "承認者" (userApprover <$> muser)
    <*> areq boolField "管理者権限" (userAdmin <$> muser)


getUserR :: UserId -> Handler Html
getUserR userId = do
    muser <- runDB $ get userId
    (form, enctype) <- generateFormPost $ userFormUpdate muser
    defaultLayout $ do
        $(widgetFile "user")

-- | フォームに記載された情報で、データベースのレコードを更新する。
putUserR :: UserId -> Handler Html
putUserR userId = do
    user <- runDB $ get404 userId
    ((res,form), enctype) <- runFormPost $ userFormUpdate $ Just user
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
    redirect RootR
