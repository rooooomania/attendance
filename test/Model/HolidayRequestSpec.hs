module Model.HolidayRequestSpec (spec) where

import TestImport
import Factories
import Model.HolidayRequest

spec :: Spec
spec = do
    withApp $ do
        describe "applyfilter" $ do
            it "対象となる休暇申請が条件に合致していればTrueを返す" $ do
                user <- runDB $ createUser "invaliduser" True
                let req  = buildRequest
                    parm = RequestParams
                        { paramFrom = Just $ utctDay testUTCTime
                        , paramTo   = Nothing
                        , paramStatus = Nothing
                        }
                -- liftIO $ print req
                applyfilter parm req `shouldBe` True
