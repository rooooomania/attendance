module Model.RequestDetail where

import Import.NoFoundation
import Foundation

createRequestDetail :: [Day] -> UserId -> HolidayRequestId -> YesodDB App ()
createRequestDetail days uid rid =
    forM_ days (\date -> do
        insert $ RequestDetail date "am" uid rid
        insert $ RequestDetail date "pm" uid rid
        return ()
        )
