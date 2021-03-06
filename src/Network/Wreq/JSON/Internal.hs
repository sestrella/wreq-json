{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE UndecidableInstances  #-}

module Network.Wreq.JSON.Internal
  ( get
  , getWith
  , post
  , postWith
  , put
  , putWith
  , delete
  , deleteWith
  ) where

import           Control.Exception
import           Control.Monad.Except
import           Data.Aeson
import           Data.ByteString.Lazy
import qualified Data.Text               as T
import qualified Network.Wreq            as W
import           Network.Wreq.JSON.Types

get :: MonadRequest m a => a -> m (Response a)
get request = do
  url <- getURL request
  run $ W.get url

getWith :: (MonadRequest m a, ToOptions m a) => a -> m (Response a)
getWith request = do
  options <- toOptions request
  url <- getURL request
  run $ W.getWith options url

post :: (MonadRequest m a, ToJSON a) => a -> m (Response a)
post request = do
  url <- getURL request
  run $ W.post url (toJSON request)

postWith :: (MonadRequest m a, MonadOptions m a) => a -> m (Response a)
postWith request = do
  options <- toOptions request
  url <- getURL request
  run $ W.postWith options url (toJSON request)

put :: (MonadRequest m a, ToJSON a) => a -> m (Response a)
put request = do
  url <- getURL request
  run $ W.put url (toJSON request)

putWith :: (MonadRequest m a, MonadOptions m a) => a -> m (Response a)
putWith request = do
  options <- toOptions request
  url <- getURL request
  run $ W.putWith options url (toJSON request)

delete :: MonadRequest m a => a -> m (Response a)
delete request = do
  url <- getURL request
  run $ W.delete url

deleteWith :: (MonadRequest m a, MonadOptions m a) => a -> m (Response a)
deleteWith request = do
  options <- toOptions request
  url <- getURL request
  run $ W.deleteWith options url

customMethod :: (MonadRequest m a) => String -> a -> m (Response a)
customMethod method request = do
  url <- getURL request
  run $ W.customMethod method url

run :: MonadResponse m a => IO (W.Response ByteString) -> m (Response a)
run request = do
  eitherResponse <- liftIO $ try request
  case eitherResponse of
    Left httpError -> throwError $ ClientHttpError httpError
    Right response -> do
      case fromResponse response of
        Left decodeError -> throwError $ ClientDecodeError decodeError
        Right result     -> return result

getURL :: ToURL m a => a -> m String
getURL = fmap (T.unpack . T.intercalate "/") . toURL
