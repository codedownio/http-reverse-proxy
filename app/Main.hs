{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Data.String.Interpolate
import Network.HTTP.Client (defaultManagerSettings, newManager)
import Network.HTTP.ReverseProxy
import qualified Network.HTTP.Types as N
import Network.Wai
import Network.Wai.Handler.Warp


port, proxyToPort :: Int
port = 3006
proxyToPort = 3005

main :: IO ()
main = do
  putStrLn [i|Running Warp proxy server on port #{port}, proxying to port #{proxyToPort}|]

  manager <- newManager defaultManagerSettings

  run port $
    waiProxyTo (\_req -> pure (WPRProxyDest (ProxyDest "localhost" proxyToPort)))
               (\e _req respond -> respond $ responseLBS N.status500 [] [i|Proxy exception: #{e}|])
               manager
