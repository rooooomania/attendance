{-# LANGUAGE OverloadedStrings #-}
module SESCreds where

import Data.ByteString.Char8 (ByteString)
import Data.Text

access, secret, siteAdmin  :: ByteString
access = "AKIAJAHK2HTHNJB2CIPA"
secret = "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"
siteAdmin = "roooooomania@gmail.com"

region :: Text
region = "us-west-2"
