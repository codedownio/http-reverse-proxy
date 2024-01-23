{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE TupleSections         #-}
{-# LANGUAGE CPP                   #-}
module Network.HTTP.ReverseProxy
    ( -- * Types
      ProxyDest (..)
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

import           Network.HTTP.ReverseProxy.Raw
import           Network.HTTP.ReverseProxy.Types
import           Network.HTTP.ReverseProxy.WAI
import           Network.HTTP.ReverseProxy.WAI.Types
