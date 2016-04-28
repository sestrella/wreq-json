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
import qualified Network.Wreq            as W
import           Network.Wreq.JSON.Types

get :: MonadRequest m a => a -> m (Response a)
get = run . W.get . url

getWith :: (MonadRequest m a, ToOptions m a) => a -> m (Response a)
getWith request = do
  options <- toOptions request
  run $ W.getWith options (url request)

post :: (MonadRequest m a, ToJSON a) => a -> m (Response a)
post request = run $ W.post (url request) (toJSON request)

run :: MonadResponse m a => IO (W.Response ByteString) -> m (Response a)
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
