{-# LANGUAGE DeriveGeneric         #-}

module Network.HTTP.ReverseProxy.Types where

import           Data.ByteString                      (ByteString)
import           GHC.Generics                         (Generic)


-- | Host\/port combination to which we want to proxy.
data ProxyDest = ProxyDest
    { pdHost :: !ByteString
    , pdPort :: !Int
    } deriving (Read, Show, Eq, Ord, Generic)

data ProxyDestUnix = ProxyDestUnix {
      pdSocketPath :: FilePath
    } deriving (Read, Show, Eq, Ord, Generic)
