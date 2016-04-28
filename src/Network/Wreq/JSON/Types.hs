{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Wreq.JSON.Types where

import           Control.Lens
import           Data.Aeson
import           Data.Aeson
import           Data.ByteString.Lazy
import           Data.Text
import qualified Network.Wreq         as W

data family Response a

class ToURL a where
  toURL :: a -> [Text]

class FromResponse a where
  fromResponse :: (W.Response ByteString) -> Either String a

instance FromJSON a => FromResponse a where
  fromResponse response = eitherDecode (response ^. W.responseBody)

