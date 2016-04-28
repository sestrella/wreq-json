{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

module Network.Wreq.JSONSpec where

import           Control.Monad.Except
import           Data.Aeson
import           Data.Text
import           Network.Wreq.JSON
import           Test.Hspec

data Get = Get

instance ToURL Get where
  toURL _ = ["http://httpbin.org", "get"]

data instance Response Get = GetResponse
  { getResponseUrl :: Text
  } deriving (Eq, Show)

instance FromJSON (Response Get) where
  parseJSON =
    withObject "GetResponse" $ \o ->
      GetResponse <$> o .: "url"

data Post = Post

instance ToURL Post where
  toURL _ = ["http://httpbin.org", "post"]

instance ToJSON Post where
  toJSON _ = object []

data instance Response Post = PostResponse
  { postResponseUrl :: Text
  } deriving (Eq, Show)

instance FromJSON (Response Post) where
  parseJSON =
    withObject "PostResponse" $ \o ->
      PostResponse <$> o .: "url"

spec :: Spec
spec = do
  describe "get" $
    it "returns the requested url" $
      runExceptT (get Get)
        `shouldReturn`
          Right (GetResponse "http://httpbin.org/get")

  describe "post" $
    it "returns the requested url" $
      runExceptT (post Post)
        `shouldReturn`
          Right (PostResponse "http://httpbin.org/post")
