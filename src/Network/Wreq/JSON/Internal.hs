{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Wreq.JSON.Internal where

import           Data.Aeson
import qualified Data.Text               as T
import qualified Network.Wreq            as W
import           Network.Wreq.JSON.Types

get :: (ToURL a, FromResponse (Response a))
    => a
    -> IO (Either String (Response a))
get = fmap fromResponse . W.get . url

post :: (ToURL a, ToJSON a, FromResponse (Response a))
     => a
     -> IO (Either String (Response a))
post request = fmap fromResponse $ W.post (url request) (toJSON request)

url :: ToURL a => a -> String
url = T.unpack . T.intercalate "/" . toURL
