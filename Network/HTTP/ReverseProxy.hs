
module Network.HTTP.ReverseProxy (
  -- * Types
  proxyDestTcp
  , proxyDestUnix

  -- * Raw
  , rawProxyTo
  , rawTcpProxyTo

  -- * WAI + http-conduit
  , waiProxyTo
  , defaultOnExc
  , waiProxyToSettings
  , WaiProxyResponse (..)

  -- ** Settings
  , WaiProxySettings
  , defaultWaiProxySettings
  , wpsOnExc
  , wpsTimeout
  , wpsSetIpHeader
  , wpsProcessBody
  , wpsUpgradeToRaw
  , wpsGetDest
  , wpsLogRequest
  , SetIpHeader (..)

  -- *** Local settings
  , LocalWaiProxySettings
  , defaultLocalWaiProxySettings
  , setLpsTimeBound
  {- FIXME
    -- * WAI to Raw
  , waiToRaw
  -}
  ) where

import Data.ByteString (ByteString)

import Network.HTTP.ReverseProxy.Raw
import Network.HTTP.ReverseProxy.Types
import Network.HTTP.ReverseProxy.WAI
import Network.HTTP.ReverseProxy.WAI.Types

-- | TCP proxy destination (host/port)
proxyDestTcp :: ByteString -> Int -> ProxyDest
proxyDestTcp = ProxyDestTcp

-- | Unix socket proxy destination (socket file path)
proxyDestUnix :: FilePath -> ProxyDest
proxyDestUnix = ProxyDestUnix
