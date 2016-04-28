{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Network.Wreq.JSONSpec where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.Aeson
import           Data.ByteString
import           Data.Text
import qualified Network.Wreq         as W
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

data BasicAuth = BasicAuth

instance ToURL BasicAuth where
  toURL _ = ["http://httpbin.org", "basic-auth", "user", "password"]

instance MonadReader (ByteString, ByteString) m => ToOptions BasicAuth m where
  toOptions _ = do
    (user, password) <- ask
    return $ W.defaults & W.auth ?~ W.basicAuth user password

data instance Response BasicAuth = BasicAuthResponse
  { basicAuthResponseAuthenticated :: Bool
  } deriving (Eq, Show)

instance FromJSON (Response BasicAuth) where
  parseJSON =
    withObject "BasicAuthResponse" $ \o -> do
      BasicAuthResponse <$> o .: "authenticated"

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

  describe "getWith" $
    it "authenticates using basic auth" $ do
      runReaderT (runExceptT (getWith BasicAuth)) ("user", "password")
        `shouldReturn`
          Right (BasicAuthResponse True)

  describe "post" $
    it "returns the requested url" $
      runExceptT (post Post)
        `shouldReturn`
          Right (PostResponse "http://httpbin.org/post")
