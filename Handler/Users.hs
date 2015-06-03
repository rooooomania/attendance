module Handler.Users where

import Import


data UserRegist = UserRegist
    { email :: !Text
    , password :: Maybe Text
    , name :: Maybe Text
    , nicname :: Maybe Text
    , approver :: Maybe (Entity User)
    , holidays :: Double
    } deriving Eq

userForm :: Form UserRegist
userForm = renderDivs $ UserRegist
    <$> areq uniqueEmailField "email" Nothing
    <*> aopt passwordField "パスワード" Nothing
    <*> aopt textField "氏名" Nothing
    <*> aopt textField "ニックネーム" Nothing
    <*> aopt (selectField approvers) "承認者" Nothing
    <*> areq doubleField "有給休暇（初回付与分）" (Just 20.0)
    where
        -- drop-down リストを動的に作るための関数が`optionsPersist`
        -- 本当は氏名を表示したかったが、氏名はMaybeなので取り急ぎ `ident` を使う
        approvers = optionsPersist [] [Asc UserIdent] userIdent
        -- 登録アドレスの重複チェックを行うよう、`custom field` をつくる
        uniqueEmailField = checkM isDuplicated emailField
        isDuplicated email = runDB $ do
            muser <- getBy $ UniqueUser email
            case muser of
                Just _ -> return $ Left ("すでに登録済みのユーザです" :: Text)
                Nothing -> return $ Right email

-- | 登録済みユーザの一覧と、ユーザ登録フォームを表示する
getUsersR :: Handler Html
getUsersR = do
    -- user テーブルから、ユーザ情報を引いてくる
    users <- runDB $ do
        usersList <- selectList [] [Asc UserFullName]
        let uids = map entityKey usersList
        let userValues = map entityVal usersList
        return $ zip uids userValues
    (form, enctype) <- generateFormPost userForm
    defaultLayout $ do
        $(widgetFile "users")

-- | ユーザを登録し、ルート画面にリダイレクトする
postUsersR :: Handler Html
postUsersR = do
    ((res, form), enctype) <- runFormPost userForm
    case res of
        FormSuccess userRegist -> do
            -- User テーブルにレコードを作成する
            let ident = email userRegist
            let pw = password userRegist
            let fn = name userRegist
            let nn = nicname userRegist
            let Just (Entity uid uval) = approver userRegist
            let hs = holidays userRegist
            userId <- runDB $ insert $
                User ident pw fn nn (Just uid) False
            Entity hid _ <- runDB $ getBy404 $ UniqueHoliday "有給休暇"
            _ <- runDB $ insert $
                HolidayBalance hs userId hid
            setMessage $ toHtml $ "ユーザを作成しました: " ++ ident
            redirect UsersR
        _ -> do
            setMessage $ toHtml ("入力に誤りがあります" :: Text)
            defaultLayout $ [whamlet|
<form method=post enctype=#{enctype}>
    ^{form}
    <button type=submit>登録
|]

                -- $(widgetFile "users")
