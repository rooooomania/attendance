module Handler.Search where

import           Import
import qualified Handler.Requests as R
import Database.Persist.Sql(fromSqlKey)
import Handler.Common(IsActiveTab(..), showPortalTab)


data SearchForm = SearchForm
    { from :: Maybe Day
    , to   :: Maybe Day
    , status  :: Maybe (Entity ApproveStatus)
    } deriving Show

requestSearchForm :: AForm Handler SearchForm
requestSearchForm = SearchForm
    <$> aopt dayField (bfs ("From" :: Text)) Nothing
    <*> aopt dayField (bfs ("To" :: Text)) Nothing
    <*> aopt (selectField approveStatus) (bfs ("ステータス" :: Text)) Nothing
  where
    approveStatus = optionsPersist [] [Asc ApproveStatusId] approveStatusName

-- パラメータを受け取り、クエリに反映させた形で休暇申請リストを返す
-- from:   休暇日from
-- to:     休暇日To
-- status: 承認ステータス
getSearchR :: Handler Html
getSearchR = do
    ((res, formWidget), enctype) <- runFormGet $ renderBootstrap3 BootstrapBasicForm requestSearchForm
    case res of
        FormMissing    -> do
            defaultLayout $ do
                showPortalTab Admin
                [whamlet|
<div .row>
    <div .col-sm-4>
        <div .panel .panel-primary>
            <div .panel-heading>休暇申請検索
            <div .panel-body>
                <form role=form method=get enctype=#{enctype}>
                    ^{formWidget}
                    <button type=submit .btn .btn-primary>検索する
|]
        FormSuccess sf -> do
            -- 絞り込み用のヘルパ関数を使う
            reqs <- runDB $ requestsWithParams $ RequestParams (from sf) (to sf) (status sf)
            defaultLayout $ do
                showPortalTab Admin
                [whamlet|
<div .row>
    <div .col-sm-4>
        <div .panel .panel-primary>
            <div .panel-heading>休暇申請検索
            <div .panel-body>
                <form role=form method=get enctype=#{enctype}>
                    ^{formWidget}
                    <button type=submit .btn .btn-primary>検索する
<div .row>
    <div .col-sm-12>
        <div .panel .panel-primary>
            <div .panel-heading>休暇申請一覧
            <div .panel-body>
                <table .table .table-hover>
                    <thead>
                        <th>申請ID
                        <th>申請者
                        <th>日数
                        <th>From
                        <th>To
                        <th>休暇カテゴリ
                        <th>状況
                        <th>申請日時
                    <tbody>
                    $forall Entity hid (HolidayRequest days from to _category status createdAt uid)<- reqs
                        <tr>
                            <td>
                                <a href=@{RequestR hid}>#{show (fromSqlKey hid)}
                            ^{R.widgetUserIdent uid}
                            <td>#{days}
                            <td>#{show from}
                            <td>#{show to}
                            <td>
                            ^{R.widgetApproveStatus status}
                            ^{R.widgetUTCTime createdAt}
|]
        _ -> do
            setMessage $ toHtml ("入力に誤りがあります" :: Text)
            redirect SearchR
