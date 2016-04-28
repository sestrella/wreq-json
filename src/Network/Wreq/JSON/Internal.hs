{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Network.Wreq.JSON.Internal where

import qualified Data.Text               as T
import qualified Network.Wreq            as W
import           Network.Wreq.JSON.Types

get :: (ToURL a, FromResponse (Response a)) => a -> IO (Either String (Response a))
get = fmap fromResponse . W.get . T.unpack . T.intercalate "/" . toURL
