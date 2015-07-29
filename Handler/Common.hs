-- | Common handler functions.
module Handler.Common where

import Data.FileEmbed (embedFile)
import Import
import Database.Persist.Sql(toSqlKey)

-- These handlers embed files in the executable at compile time to avoid a
-- runtime dependency, and for efficiency.

getFaviconR :: Handler TypedContent
getFaviconR = return $ TypedContent "image/x-icon"
                     $ toContent $(embedFile "config/favicon.ico")

getRobotsR :: Handler TypedContent
getRobotsR = return $ TypedContent typePlain
                    $ toContent $(embedFile "config/robots.txt")

-- helper function originaly created.

data IsActiveTab = Portal | Approver | Admin deriving Eq
showPortalTab :: IsActiveTab -> Widget
showPortalTab tab = do
    (uid, u, cnt) <- handlerToWidget $ do
        uid <- requireAuthId
        u <- runDB $ get404 uid
        cnt <- runDB $ do
            cnts <- selectList [UserApprover ==. Just uid] [] >>= mapM (\(Entity uid' u') -> do
                count [HolidayRequestUser ==. uid', HolidayRequestStatus ==. toSqlKey 1]
                )
            return $ sum cnts
        return (uid, u, cnt)
    [whamlet|
<div .panel .panel-default>
    <ul .nav .nav-pills>
        $if tab == Portal
            <li .active>
                <a href=@{RootR}>top
        $else
            <li>
                <a href=@{RootR}>top
        $if tab == Approver
            $if cnt > 0
                <li .active>
                    <a href=@{ApproverR uid}>approver #
                        <span .badge>#{cnt}
        $elseif cnt > 0
                <li>
                    <a href=@{ApproverR uid}>approver #
                        <span .badge>#{cnt}
        $if tab == Admin
            $if userAdmin u
            <li .active>
                <a href=@{SearchR}>admin
        $else
            <li>
                <a href=@{SearchR}>admin
|]
