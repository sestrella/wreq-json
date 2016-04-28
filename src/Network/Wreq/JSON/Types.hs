{-# LANGUAGE ConstraintKinds       #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Network.Wreq.JSON.Types where

import           Control.Lens
import           Control.Monad.Except
import           Data.Aeson
import           Data.ByteString.Lazy
import           Data.Text
import           Network.HTTP.Client  (HttpException)
import qualified Network.Wreq         as W

type MonadClient m = (MonadIO m, MonadError ClientError m)
type MonadResponse m a = (MonadClient m, FromResponse (Response a))
type MonadRequest m a = (MonadResponse m a, ToURL m a)
type MonadOptions m a = (ToOptions m a, ToJSON a)

data ClientError
  = ClientHttpError HttpException
  | ClientDecodeError String
  deriving Show

instance Eq ClientError where
  (==) = undefined

data family Response a

class Monad m => ToURL m a where
  toURL :: a -> m [Text]

class Monad m => ToOptions m a where
  toOptions :: a -> m W.Options
  toOptions _ = return W.defaults

class FromResponse a where
  fromResponse :: (W.Response ByteString) -> Either String a

instance FromJSON a => FromResponse a where
  fromResponse response = eitherDecode (response ^. W.responseBody)
