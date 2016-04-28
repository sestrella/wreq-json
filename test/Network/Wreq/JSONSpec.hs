{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Network.Wreq.JSONSpec where

import           Data.Aeson
import           Network.Wreq.JSON
import           Test.Hspec

data Get = Get

instance ToURL Get where
  toURL _ = ["http://httpbin.org", "get"]

data instance Response Get = GetResponse
  { getResponseUrl :: String
  } deriving (Eq, Show)

instance FromJSON (Response Get) where
  parseJSON =
    withObject "Response Get" $ \o ->
      GetResponse <$> o .: "url"

spec :: Spec
spec =
  describe "get" $
    it "returns the requested url" $
      get Get `shouldReturn` Right (GetResponse "http://httpbin.org/get")
