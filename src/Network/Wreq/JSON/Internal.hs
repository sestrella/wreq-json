{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Network.Wreq.JSON.Internal where

import           Control.Exception
import           Control.Monad.Except
import           Data.Aeson
import           Data.ByteString.Lazy
import qualified Data.Text               as T
import           Network.HTTP.Client     (HttpException)
import qualified Network.Wreq            as W
import           Network.Wreq.JSON.Types

data ClientError
  = ClientHttpError HttpException
  | ClientDecodeError String
  deriving Show

instance Eq ClientError where
  (==) = undefined

get :: (ToURL a, FromResponse (Response a), MonadIO m, MonadError ClientError m)
    => a
    -> m (Response a)
get = run . W.get . url

post :: (ToURL a, ToJSON a, FromResponse (Response a), MonadIO m, MonadError ClientError m)
     => a
     -> m (Response a)
post request = run $ W.post (url request) (toJSON request)

run :: (FromResponse (Response a), MonadIO m, MonadError ClientError m)
    => IO (W.Response ByteString)
    -> m (Response a)
run request = do
  eitherResponse <- liftIO $ try request
  case eitherResponse of
    Left httpError -> throwError $ ClientHttpError httpError
    Right response -> do
      case fromResponse response of
        Left decodeError -> throwError $ ClientDecodeError decodeError
        Right result     -> return result

url :: ToURL a => a -> String
url = T.unpack . T.intercalate "/" . toURL
