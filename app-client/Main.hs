{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad
import Data.String.Interpolate
import Network.HTTP.Client
import qualified Network.HTTP.Types as N


proxyToPort :: Int
proxyToPort = 3005

main :: IO ()
main = do
  manager <- newManager defaultManagerSettings

  forM_ [[i|http://localhost:#{proxyToPort}/simple|], [i|http://localhost:#{proxyToPort}/early_hints|]] $ \url -> do
    putStrLn [i|Doing an HTTP request to #{url}|]
    request <- parseRequest url
    response <- httpLbs request manager
    putStrLn ("The status code was: " ++ show (N.statusCode $ responseStatus response))
    putStrLn ("Full response: " ++ show response)
    print $ responseBody response
