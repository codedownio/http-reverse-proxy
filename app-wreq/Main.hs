{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Control.Monad
import Data.String.Interpolate
import Network.Wreq


proxyToPort :: Int
proxyToPort = 3005

main :: IO ()
main = do
  forM_ [[i|http://localhost:#{proxyToPort}/simple|], [i|http://localhost:#{proxyToPort}/early_hints|]] $ \url -> do
    putStrLn [i|Doing an HTTP request to #{url}|]
    r <- get url
    putStrLn [i|Result: #{r}|]
