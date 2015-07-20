module Import.SendMail where

import Foundation
import Import.NoFoundation

import Network.Mail.Mime
import Network.Mail.Mime.SES
import System.Random (newStdGen)
import qualified Data.ByteString.Lazy.UTF8 as LU
import           Text.Blaze.Html.Renderer.Utf8 (renderHtml)

import SESCreds

-- ==========================================================
-- | 将来的にはパッケージ化したいが、やり方がわからないのでこのファイルにベタ書きする
-- SendEmail.hs

-- | 対象の申請者に対して、承認結果を知らせるメールを配信する
sendMailForUser :: Text -> Entity User -> Handler ()
sendMailForUser result user = do
    let email = (userIdent . entityVal) user
    let ses = SES
          { sesFrom = siteAdmin
          , sesTo = [encodeUtf8 email]
          , sesAccessKey = access
          , sesSecretKey = secret
          , sesRegion = region
          }
    h <- getYesod
    render <- getUrlRender
    let rootr = render RootR
    renderSendMailSES (appHttpManager h) ses Mail
        { mailHeaders =
            [("Subject", "no reply")]
        , mailFrom = Address Nothing "rooooomania@gmail.com"
        , mailTo = [Address Nothing email]
        , mailCc = []
        , mailBcc = []
        , mailParts = return
            [ textPart
            , Part
               { partType = "text/html; charset=utf-8"
               , partEncoding = None
               , partFilename = Nothing
               , partContent = renderHtml [shamlet|\
    <p>あなたの休暇申請が #{result} されました。
    <p>次のリンクをクリックして、内容を確認してください。
    <p>
    <a href=#{rootr}>#{rootr}
    <p> よろしくお願いいたします。
    |]
               , partHeaders = []
               }
            ]
        }
      where
          textPart = Part "text/plain" None Nothing [] $ LU.fromString $ unlines
              [ "あなたの休暇申請が？？されました。"
              , "次のリンクをクリックして、内容を確認してください。"
            --   , T.unpack rootr
              ]

-- | 対象の承認者に対して、承認を促すメールを配信する
--   メールボディには、承認者向けの承認ビューへのリンクを記載する
sendMailForApprove :: Entity User -> Handler ()
sendMailForApprove approver = do
    let email = (userIdent . entityVal) approver
    let ses = SES
              { sesFrom = "rooooomania@gmail.com"
              , sesTo = [encodeUtf8 email]
              , sesAccessKey = "AKIAJAHK2HTHNJB2CIPA"
              , sesSecretKey = "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"
              , sesRegion = "us-west-2"
              }
    h <- getYesod
    render <- getUrlRender
    let rootr = render (ApproverR $ entityKey approver)
    renderSendMailSES (appHttpManager h) ses Mail
        { mailHeaders =
            [("Subject", "no reply")]
        , mailFrom = Address Nothing "rooooomania@gmail.com"
        , mailTo = [Address Nothing email]
        , mailCc = []
        , mailBcc = []
        , mailParts = return
            [ textPart
            , Part
               { partType = "text/html; charset=utf-8"
               , partEncoding = None
               , partFilename = Nothing
               , partContent = renderHtml [shamlet|\
<p>メンバーが休暇を申請しています。内容を確認してください。
<p>次のリンクをクリックして、承認者ページに進んでください。
<p>
   <a href=#{rootr}>#{rootr}
<p> よろしくお願いいたします。
|]
               , partHeaders = []
               }
            ]
        }
      where
          textPart = Part "text/plain" None Nothing [] $ LU.fromString $ unlines
              [ "メンバーが休暇を申請しています。内容を確認してください。"
              , "次のリンクをクリックして、承認者ページに進んでください。"
            --   , T.unpack rootr
              ]

-- | 休暇申請に紐づく、承認者データを返す
myApprover :: Entity HolidayRequest -> Handler (Entity User)
myApprover (Entity rid rval) = runDB $ do
    let uid = holidayRequestUser rval
    uval <- get404 $ uid
    let (Just approverId) = userApprover uval
    Just approver <- selectFirst [UserId ==. approverId] []
    return approver
