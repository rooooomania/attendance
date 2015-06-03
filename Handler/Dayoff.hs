module Handler.Dayoff where

import Import

-- | クエリパラメータで渡されたユーザの休暇申請状況を表示する
getDayoffR :: Int -> Handler Html
getDayoffR userId = do
-- 1. ユーザ認証して、該当のユーザの操作であることを確認する（認可？）
-- 2. 申請状況を取得して、すべて表示する
-- テスト段階では、擬似モデルを作成してシミュレーションしてみる
    setMessage "this is get method."
    (dayoffWidget, enctype) <- generateFormPost $ dayoffForm userId
    defaultLayout $ do
        toWidget [lucius|
            body *{
                    font-family: 'Noto Sans Japanese', sans-serif;
            }
        |]
        addStylesheetRemote "http://fonts.googleapis.com/earlyaccess/notosansjapanese.css"
        setTitle "morimoriya"
        $(widgetFile "dayoff")
data DayoffOrder = DayoffOrder
    { orderId        :: !Int
    , dayoffFrom     :: !Text
    , dayoffTo       :: !Text
    , day            :: !Int
    , category       :: !Int
    , reason         :: !Text
    }

dayoffOrders :: [DayoffOrder]
dayoffOrders =
    [ DayoffOrder 1 "2015/01/01" "2015/01/01" 1 1 "特別休暇"
    , DayoffOrder 2 "2015/01/02" "2015/01/01" 1 2 "有給休暇"
    , DayoffOrder 3 "2015/01/03" "2015/01/01" 1 3 "有給休暇"
    , DayoffOrder 4 "2015/01/04" "2015/01/01" 1 4 "有給休暇"
    , DayoffOrder 5 "2015/01/05" "2015/01/01" 1 5 "other"
    ]

dayoffForm :: Int -> Form DayoffOrder
dayoffForm userId = renderDivs $ DayoffOrder
    <$> pure userId
    <*> areq textField  "from" Nothing
    <*> areq textField "to" Nothing
    <*> areq intField "day" (Just 1)
    <*> areq intField "category" Nothing
    <*> areq textField "reason" Nothing

-- 3. 休暇申請フォームを表示する
