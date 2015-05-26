module Handler.Test where
import Import

getTestR :: Handler Html
getTestR = defaultLayout $(widgetFile "test")

-- フォーム入力後、TestResultRへリダイレクト
postTestR :: Handler Html
postTestR = do
    lastName <- runInputPost $ ireq textField "name"
    setSession "lastName" lastName
    setMessage $ toHtml $ lastName ++ "です。"
    redirect TestResultR

getTestResultR :: Handler Html
getTestResultR = defaultLayout $ do
    -- lastNameを取得したい
    mLastName <- lookupSession "lastName"
    -- testresult をレンダリングする
    $(widgetFile "testresult")
    toWidgetHead
        [hamlet|
            <meta name=keywords content="some sample keywords">
        |]
    setTitle "morimori"
    [whamlet|
        <h2>"this element is added by whamlet."
    |]
-- widget作ってみる。めちゃくちゃな順番でも、きちんと整理されるか検証する。
