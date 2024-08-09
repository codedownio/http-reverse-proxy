{-# LANGUAGE DeriveGeneric #-}

module Network.HTTP.ReverseProxy.Types where

import Data.ByteString (ByteString)
import GHC.Generics (Generic)


-- | Destination to which we want to proxy.
data ProxyDest = ProxyDestTcp {
  pdHost :: !ByteString
  , pdPort :: !Int
  } | ProxyDestUnix {
    pdSocketPath :: FilePath
  }
  deriving (Read, Show, Eq, Ord, Generic)
