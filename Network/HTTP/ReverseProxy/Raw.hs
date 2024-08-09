{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Network.HTTP.ReverseProxy.Raw where

import           Data.ByteString                 (ByteString)
import qualified Data.ByteString                 as S
import qualified Data.ByteString.Char8           as S8
import qualified Data.CaseInsensitive            as CI
import           Data.Conduit
import qualified Data.Conduit.Network            as DCN
import qualified Data.Conduit.Network.Unix       as DCNU
import           Data.Functor.Identity           (Identity (..))
import           Data.IORef
import           Data.Maybe                      (fromMaybe)
import           Data.Streaming.Network          (HasReadWrite, readLens)
import           Data.Word8                      (isSpace, _colon, _cr)
import           Network.HTTP.ReverseProxy.Types
import qualified Network.HTTP.Types              as HT
import           UnliftIO                        (MonadIO, liftIO, MonadUnliftIO, concurrently_)


-- | Set up a reverse proxy server, which will have a minimal overhead.
--
-- This function uses raw sockets, parsing as little of the request as
-- possible. The workflow is:
--
-- 1. Parse the first request headers.
--
-- 2. Ask the supplied function to specify how to reverse proxy.
--
-- 3. Open up a connection to the given host\/port.
--
-- 4. Pass all bytes across the wire unchanged.
--
-- If you need more control, such as modifying the request or response, use 'waiProxyTo'.
rawProxyTo :: forall ad m. (
  MonadUnliftIO m, HasReadWrite ad
  ) => (HT.RequestHeaders -> m (Either (ad -> m ()) ProxyDest))
    -- ^ How to reverse proxy. A @Left@ result will run the given
    -- 'DCN.Application', whereas a @Right@ will reverse proxy to the
    -- given host\/port.
    -> ad
    -> m ()
rawProxyTo getDest appdata = do
    (rsrc, headers) <- liftIO $ fromClient $$+ getHeaders
    edest <- getDest headers
    case edest of
        Left app -> do
            -- We know that the socket will be closed by the toClient side, so
            -- we can throw away the finalizer here.
            irsrc <- liftIO $ newIORef rsrc
            let readData = do
                    rsrc1 <- readIORef irsrc
                    (rsrc2, mbs) <- rsrc1 $$++ await
                    writeIORef irsrc rsrc2
                    return $ fromMaybe "" mbs
            app $ runIdentity (readLens (const (Identity readData)) appdata)


        Right (ProxyDestTcp host port) -> liftIO $ DCN.runTCPClient (DCN.clientSettings port host) (withServer rsrc)
        Right (ProxyDestUnix socketPath) -> liftIO $ DCNU.runUnixClient (DCNU.clientSettings socketPath) (withServer rsrc)

  where
    fromClient = DCN.appSource appdata
    toClient = DCN.appSink appdata
    withServer rsrc appdataServer = concurrently_
        (rsrc $$+- toServer)
        (runConduit $ fromServer .| toClient)
      where
        fromServer = DCN.appSource appdataServer
        toServer = DCN.appSink appdataServer

-- | Set up a reverse tcp proxy server, which will have a minimal overhead.
--
-- This function uses raw sockets, parsing as little of the request as
-- possible. The workflow is:
--
-- 1. Open up a connection to the given host\/port.
--
-- 2. Pass all bytes across the wire unchanged.
--
-- If you need more control, such as modifying the request or response, use 'waiProxyTo'.
--
-- @since 0.4.4
rawTcpProxyTo :: forall ad m. (HasReadWrite ad, MonadIO m) => ProxyDest -> ad -> m ()
rawTcpProxyTo proxyDest appdata = liftIO $ case proxyDest of
  (ProxyDestTcp host port) -> DCN.runTCPClient (DCN.clientSettings port host) withServer
  (ProxyDestUnix socketPath) -> DCNU.runUnixClient (DCNU.clientSettings socketPath) withServer
  where
    withServer appdataServer = concurrently_
      (runConduit $ DCN.appSource appdata       .| DCN.appSink appdataServer)
      (runConduit $ DCN.appSource appdataServer .| DCN.appSink appdata      )

-- | Get the HTTP headers for the first request on the stream, returning on
-- consumed bytes as leftovers. Has built-in limits on how many bytes it will
-- consume (specifically, will not ask for another chunked after it receives
-- 1000 bytes).
getHeaders :: Monad m => ConduitT ByteString o m HT.RequestHeaders
getHeaders =
    toHeaders <$> go id
  where
    go front =
        await >>= maybe close push
      where
        close = leftover bs >> return bs
          where
            bs = front S8.empty
        push bs'
            | "\r\n\r\n" `S8.isInfixOf` bs
              || "\n\n" `S8.isInfixOf` bs
              || S8.length bs > 4096 = leftover bs >> return bs
            | otherwise = go $ mappend bs
          where
            bs = front bs'
    toHeaders = map toHeader . takeWhile (not . S8.null) . drop 1 . S8.lines
    toHeader bs =
        (CI.mk key, val)
      where
        (key, bs') = S.break (== _colon) bs
        val = S.takeWhile (/= _cr) $ S.dropWhile isSpace $ S.drop 1 bs'
