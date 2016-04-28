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

instance Monad m => ToURL m Get where
  toURL _ = return ["http://httpbin.org", "get"]

data instance Response Get = GetResponse Text
  deriving (Eq, Show)

instance FromJSON (Response Get) where
  parseJSON =
    withObject "GetResponse" $ \o ->
      GetResponse <$> o .: "url"

data BasicAuth = BasicAuth

instance Monad m => ToURL m BasicAuth where
  toURL _ = return ["http://httpbin.org", "basic-auth", "user", "password"]

instance MonadReader (ByteString, ByteString) m => ToOptions m BasicAuth where
  toOptions _ = do
    (user, password) <- ask
    return $ W.defaults & W.auth ?~ W.basicAuth user password

data instance Response BasicAuth = BasicAuthResponse Bool
  deriving (Eq, Show)

instance FromJSON (Response BasicAuth) where
  parseJSON =
    withObject "BasicAuthResponse" $ \o -> do
      BasicAuthResponse <$> o .: "authenticated"

data Post = Post

instance Monad m => ToURL m Post where
  toURL _ = return ["http://httpbin.org", "post"]

instance ToJSON Post where
  toJSON _ = object []

data instance Response Post = PostResponse Text
  deriving (Eq, Show)

instance FromJSON (Response Post) where
  parseJSON =
    withObject "PostResponse" $ \o ->
      PostResponse <$> o .: "url"

data PostWithHeader = PostWithHeader

instance Monad m => ToURL m PostWithHeader where
  toURL _ = return ["http://httpbin.org", "post"]

instance MonadReader ByteString m => ToOptions m PostWithHeader where
  toOptions _ = do
    header <- ask
    return $ W.defaults & W.header "Custom-Header" .~ [header]

instance ToJSON PostWithHeader where
  toJSON _ = object []

data CustomHeader = CustomHeader Text
  deriving (Eq, Show)

data instance Response PostWithHeader = PostWithHeaderResponse CustomHeader
  deriving (Eq, Show)

instance FromJSON CustomHeader where
  parseJSON =
    withObject "CustomHeader" $ \o ->
      CustomHeader <$> o .: "Custom-Header"

instance FromJSON (Response PostWithHeader) where
  parseJSON =
    withObject "PostWithHeaderResponse" $ \o ->
      PostWithHeaderResponse <$> o .: "headers"

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

  describe "postWith" $
    it "returns the header sent" $ do
      runReaderT (runExceptT (postWith PostWithHeader)) "value"
        `shouldReturn`
          Right (PostWithHeaderResponse (CustomHeader "value"))
