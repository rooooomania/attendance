module Handler.RootSpec (spec) where

import           Factories
import           Prelude    (head)
import           TestImport
spec :: Spec
spec = do
    withApp $ do
        describe  "Get RootR" $ do
            it "未認証でサイトアクセスすると、ログイン画面にリダイレクトする" $ do
                user <- runDB $ createUser "invaliduser" True
                get RootR

                -- getResponseを使えば、`location`ヘッダの中身を見られる。これを使えばアサートできる
                -- mres <- getResponse
                -- case mres of
                --     Just res -> liftIO $ putStrLn . pack $ show res
                --     _ -> liftIO $ putStrLn . pack $ "error!!"
                statusIs 303


            it "認証に成功すれば、ステータスコード200が返り、ログイン成功のメッセージが表示される" $ do
                user <- runDB $ createUser "rooooomania" True
                holiday <- runDB createMaster
                authenticateAs $ user
                get RootR

                htmlAllContain "#message" "You are now logged in"
                statusIs 200

            context "休暇申請情報がなければ" $ do
                it "申請状況一覧にはなにも表示されない" $ do
                    user <- runDB $ createUser "rooooomania" True
                    holiday <- runDB createMaster
                    authenticateAs user
                    get RootR

                    statusIs 200
                    htmlCount "table.table td" 0

            it "休暇残高（有給休暇）を表示している" $ do
                user <- runDB $ createUser "rooooomania" True
                holiday <- runDB createMaster
                runDB $ createHolidayBalance user holiday
                authenticateAs user
                get RootR

                htmlAnyContain "td" "20.0"

            it "休暇申請フォームを表示している" $ do
                user <- runDB $ createUser "rooooomania" True
                holiday <- runDB createMaster
                authenticateAs user
                get RootR

                htmlAnyContain ".panel-heading" "休暇申請"

        describe "POST RootR" $ do
            it "認証されていないリクエストは303を与え、ログイン画面に遷移させる" $ do
                -- ログイン画面にリダイレクトするため、401は返らない。。
                post RootR

                statusIs 303

            it "休暇申請すれば、その分のリストを表示している" $ do
                user <- runDB $ createUser "rooooomania" True
                holiday <- runDB createMaster
                runDB $ createHolidayBalance user holiday
                authenticateAs user
                runDB $ insert buildRequest
                get RootR

                htmlAnyContain ".panel-heading" "申請状況"

            it "休暇申請レコードを作成し、RootRにリダイレクトする" $ do
                user <- runDB $ createUser "rooooomania" True
                holiday <- runDB createMaster
                runDB $ createHolidayBalance user holiday

                authenticateAs user

                get RootR
                request $ do
                    addToken
                    byLabel "From" "2015/06/01"
                    byLabel "To" "2015/06/01"
                    byLabel "休暇区分" "1" -- pulldown list の場合、optional属性 の value を入れる
                    setMethod "POST"
                    setUrl RootR
                rs <- runDB $ selectList [] [Asc HolidayRequestId]
                let days = holidayRequestDays $ entityVal $ Prelude.head rs

                days `shouldBe` 1.0
                statusIs 303

            context "申請自と至を逆にすると" $ do
                it "申請期間に関するメッセージを表示する" $ do
                    user <- runDB $ createUser "rooooomania" True
                    holiday <- runDB createMaster
                    runDB $ createHolidayBalance user holiday

                    authenticateAs user

                    get RootR
                    request $ do
                        addToken
                        byLabel "From" "2015/06/02"
                        byLabel "To" "2015/06/01"
                        byLabel "休暇区分" "1" -- pulldown list の場合、optional属性 の value を入れる
                        setMethod "POST"
                        setUrl RootR

                    get RootR -- 実際はリダイレクトされているので
                    htmlAllContain "#message" "申請期間に誤りがあります"
            context "休暇申請残高が不足していると" $ do
                it "不足に関するメッセージを表示する" $ do
                    user <- runDB $ createUser "rooooomania" True
                    holiday <- runDB createMaster
                    runDB $ createHolidayBalance user holiday

                    authenticateAs user

                    get RootR
                    request $ do
                        addToken
                        byLabel "From" "2015/06/02"
                        byLabel "To" "2015/08/01"
                        byLabel "休暇区分" "1" -- pulldown list の場合、optional属性 の value を入れる
                        setMethod "POST"
                        setUrl RootR

                    get RootR -- 実際はリダイレクトされているので
                    htmlAllContain "#message" "そんなに休めませんよ"

        describe "POST CancelR" $ do
            it "承認ステータスを取消に変更する" $ do
                user <- runDB $ createUser "rooooomania" True
                holiday <- runDB createMaster
                runDB $ createHolidayBalance user holiday

                authenticateAs user

                get RootR
                request $ do
                    addToken
                    byLabel "From" "2015/06/01"
                    byLabel "To" "2015/06/01"
                    byLabel "休暇区分" "1" -- pulldown list の場合、optional属性 の value を入れる
                    setMethod "POST"
                    setUrl RootR
                rs <- runDB $ selectList [] [Asc HolidayRequestId]

                let hid = entityKey $ Prelude.head rs

                request $ do
                    setMethod "PUT"　-- フォームからはPOSTしているが、実態はPUT
                    setUrl $ CancelR (entityKey user) hid

                statusIs 303
                get RootR　-- 上のputメソッドは、303を返すまでが仕事で、ブラウザのようにリダイレクトはしないため、明示的なGETが必要
                htmlAnyContain "table > tbody > tr > td" "20.0" -- 一旦 19.0 になって、取り消しにより回復
                htmlAnyContain "span" "不可"

                -- データベースが正しく更新されているか、というテストは
                -- ほんとうは unit test としてModelディレクトリ以下で行いたい
                sts <- runDB $
                    get404 hid >>= get404 . holidayRequestStatus >>= return . approveStatusName
                sts `shouldBe` "取消"

            it "該当の休暇明細を削除する" $ do
                user <- runDB $ createUser "rooooomania" True
                holiday <- runDB createMaster
                runDB $ createHolidayBalance user holiday

                authenticateAs user

                get RootR
                request $ do
                    addToken
                    byLabel "From" "2015/06/01"
                    byLabel "To" "2015/06/01"
                    byLabel "休暇区分" "1" -- pulldown list の場合、optional属性 の value を入れる
                    setMethod "POST"
                    setUrl RootR
                rs <- runDB $ selectList [] [Asc HolidayRequestId]

                let hid = entityKey $ Prelude.head rs

                request $ do
                    setMethod "PUT"　-- フォームからはPOSTしているが、実態はPUT
                    setUrl $ CancelR (entityKey user) hid

                cnt <- runDB $ count [RequestDetailUser ==. entityKey user]
                cnt `shouldBe` 0
    describe "PENDING" $ do
            it "認証に失敗すると、404を与える" $ do
                pendingWith "第三者認証の場合は認証失敗の制御をアプリが行わないため、失敗した結果が送信されない"
            it "休暇残高を補正する" $ do
                pendingWith "モデル部のテストなのでできれば単体テスト"
