module Factories where

import TestImport
import Data.Time.Clock(secondsToDiffTime)
import Database.Persist.Sql(fromSqlKey, toSqlKey, SqlPersistM)
import Yesod (get404, getBy404)


buildUser :: User
buildUser = User
    { userIdent = "testuser@gmail.com"
    , userPassword = Nothing
    , userFullName = Nothing
    , userNicname = Nothing
    , userApprover = Nothing
    , userAdmin = False
    }

testUTCTime :: UTCTime
testUTCTime  = UTCTime (fromGregorian 2015 06 10) (secondsToDiffTime 0)

buildRequest :: HolidayRequest
buildRequest = HolidayRequest
    { holidayRequestDays = 1
    , holidayRequestWhenFrom = fromGregorian 2015 06 01
    , holidayRequestWhenTo = fromGregorian 2015 06 01
    , holidayRequestCategory = toSqlKey 1
    , holidayRequestStatus = toSqlKey 1
    , holidayRequestCreatedAt = testUTCTime
    , holidayRequestUser = toSqlKey 1
    }

buildBalance :: HolidayBalance
buildBalance = HolidayBalance
    { holidayBalanceBalance = 20.0
    , holidayBalanceUser = toSqlKey 1
    , holidayBalanceCategory = toSqlKey 1
    }

createMaster :: SqlPersistM (Entity Holiday)
createMaster = do
    -- ApproveStatus
    insert ApproveStatus { approveStatusName = "申請中"}
    insert ApproveStatus { approveStatusName = "取消"}
    -- Holiday
    insertEntity Holiday
        { holidayName = "有給休暇"
        , holidayDays = 20.0
        }

createHolidayBalance :: Entity User -> Entity Holiday -> SqlPersistM ()
createHolidayBalance u h = do
    insert_ HolidayBalance
        { holidayBalanceBalance = 20.0
        , holidayBalanceUser = entityKey u
        , holidayBalanceCategory = entityKey h
        }

createUser :: Text -> Bool -> SqlPersistM (Entity User)
createUser user admin = do
    let u = buildUser
    uid <- insert $ buildUser
        { userIdent = user ++ "@gmail.com"
        , userAdmin = admin
        }
    update uid [UserApprover =. Just uid]

    u <- get404 uid
    return $ Entity uid u

createRequest :: Entity User -> SqlPersistM (Entity HolidayRequest)
createRequest = undefined
