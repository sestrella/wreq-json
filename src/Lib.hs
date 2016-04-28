{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib where

import           Control.Lens
import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Network.Wreq         as W

data family Response a

class ToURL a where
  toURL :: a -> [Text]

class FromResponse a where
  fromResponse :: (W.Response ByteString) -> Either String a

instance FromJSON a => FromResponse a where
  fromResponse response = eitherDecode (response ^. W.responseBody)

get :: (ToURL a, FromResponse (Response a)) => a -> IO (Either String (Response a))
get = fmap fromResponse . W.get . T.unpack . T.intercalate "/" . toURL
