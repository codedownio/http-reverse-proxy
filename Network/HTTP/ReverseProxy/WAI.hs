{-# LANGUAGE CPP                 #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

module Network.HTTP.ReverseProxy.WAI where

import           Blaze.ByteString.Builder                     (Builder, fromByteString, toByteString, toLazyByteString)
import           Control.Applicative                          ((<|>))
import           Control.Monad                                (unless)
import           Data.ByteString                              (ByteString)
import qualified Data.ByteString                              as S
import           Data.ByteString.Builder.HTTP.Chunked         (chunkedTransferEncoding, chunkedTransferTerminator)
import qualified Data.ByteString.Char8                        as S8
import qualified Data.ByteString.Lazy                         as L
import qualified Data.CaseInsensitive                         as CI
import           Data.Conduit
import qualified Data.Conduit.List                            as CL
import qualified Data.Conduit.Network                         as DCN
import qualified Data.Conduit.Network.Unix                    as DCNU
import           Data.Maybe                                   (fromMaybe, isNothing, listToMaybe)
import           Data.Set                                     (Set)
import qualified Data.Set                                     as Set
import           Data.Streaming.Network                       (HasReadWrite)
import qualified Data.Text                                    as T
import qualified Data.Text.Encoding                           as TE
import           Network.HTTP.Client                          (BodyReader, brRead)
import qualified Network.HTTP.Client                          as HC
import           Network.HTTP.ReverseProxy.Types
import           Network.HTTP.ReverseProxy.WAI.SemiCachedBody
import           Network.HTTP.ReverseProxy.WAI.Types
import qualified Network.HTTP.Types                           as HT
import qualified Network.HTTP.Types.Header                    as H
import qualified Network.Wai                                  as WAI
import           Network.Wai.Logger                           (showSockAddr)
import           UnliftIO                                     (MonadIO, liftIO, timeout, SomeException, try, bracket, concurrently_)


-- | Creates a WAI 'WAI.Application' which will handle reverse proxies.
-- Connections to the proxied server will be provided via http-conduit. As
-- such, all requests and responses will be fully processed in your reverse
-- proxy. This allows you much more control over the data sent over the wire,
-- but also incurs overhead. For a lower-overhead approach, consider
-- 'rawProxyTo'.
--
-- Most likely, the given application should be run with Warp, though in theory
-- other WAI handlers will work as well.
--
-- Note: This function will use chunked request bodies for communicating with
-- the proxied server. Not all servers necessarily support chunked request
-- bodies, so please confirm that yours does (Warp, Snap, and Happstack, for example, do).
waiProxyTo ::
  (WAI.Request -> IO WaiProxyResponse)
  -- ^ How to reverse proxy.
  -> (SomeException -> WAI.Request -> (ByteString -> IO ()) -> IO ())
  -- ^ How to handle exceptions when calling remote server. For a
  -- simple 502 error page, use 'defaultOnExc'.
  -> HC.Manager
  -- ^ connection manager to utilize
  -> WAI.Application
waiProxyTo getDest onError = waiProxyToSettings getDest defaultWaiProxySettings { wpsOnExc = onError }


waiProxyToSettings :: (WAI.Request -> IO WaiProxyResponse)
                   -> WaiProxySettings
                   -> HC.Manager
                   -> WAI.Application
waiProxyToSettings getDest wps' manager req0 sendResponse = do
    let wps = wps'{wpsGetDest = wpsGetDest wps' <|> Just (fmap (LocalWaiProxySettings $ wpsTimeout wps',) . getDest)}
    (lps, edest') <- fromMaybe
        (const $ return (defaultLocalWaiProxySettings, WPRResponse $ WAI.responseLBS HT.status500 [] "proxy not setup"))
        (wpsGetDest wps)
        req0
    let edest =
            case edest' of
                WPRResponse res -> Left $ \_req -> ($ res)
                WPRProxyDest pd -> Right (pd, req0, False)
                WPRProxyDestSecure pd -> Right (pd, req0, True)
                WPRModifiedRequest req pd -> Right (pd, req, False)
                WPRModifiedRequestSecure req pd -> Right (pd, req, True)
                WPRApplication app -> Left app
        timeBound us f =
            timeout us f >>= \case
                Just res -> return res
                Nothing -> sendResponse $ WAI.responseLBS HT.status500 [] "timeBound"
    case edest of
        Left app -> maybe id timeBound (lpsTimeBound lps) $ app req0 sendResponse
        Right (ProxyDestTcp host port, req, secure) -> tryWebSockets wps (DCN.runTCPClient (DCN.clientSettings port host)) req sendResponse $
            proxyNormally manager wps lps (Just (host, port)) req secure sendResponse
        Right (ProxyDestUnix socketPath, req, secure) -> tryWebSockets wps (DCNU.runUnixClient (DCNU.clientSettings socketPath)) req sendResponse $
            proxyNormally manager wps lps Nothing req secure sendResponse

proxyNormally :: HC.Manager
              -> WaiProxySettings
              -> LocalWaiProxySettings
              -> Maybe (ByteString, Int)
              -> WAI.Request
              -> Bool
              -> (WAI.Response -> IO b)
              -> IO b
proxyNormally manager wps lps maybeHostPort req secure sendResponse = do
  scb <- semiCachedBody (WAI.requestBody req)
  let body =
        case WAI.requestBodyLength req of
            WAI.KnownLength i -> HC.RequestBodyStream (fromIntegral i) scb
            WAI.ChunkedBody -> HC.RequestBodyStreamChunked scb

  -- We don't want this to be used, but WAI requires us to provide a default response
  -- when using WAI.responseRaw. This should be fine because none of the functions like
  -- 'responseStatus' or 'responseHeaders' should be called on this.
  let defaultResponse = WAI.responseLBS HT.conflict409 [] mempty

  sendResponse $ flip WAI.responseRaw defaultResponse $ \_ sendToClient -> do
    let req' =
          HC.defaultRequest
            { HC.checkResponse = \_ _ -> return ()
            , HC.responseTimeout = maybe HC.responseTimeoutNone HC.responseTimeoutMicro $ lpsTimeBound lps
            , HC.method = WAI.requestMethod req
            , HC.secure = secure
            , HC.host = maybe (HC.host HC.defaultRequest) fst maybeHostPort
            , HC.port = maybe (HC.port HC.defaultRequest) snd maybeHostPort
            , HC.path = WAI.rawPathInfo req
            , HC.queryString = WAI.rawQueryString req
            , HC.requestHeaders = fixReqHeaders wps req
            , HC.requestBody = body
            , HC.redirectCount = 0
            , HC.earlyHintHeadersReceived = \headers ->
                  sendToClient $ L.toStrict $ toLazyByteString $
                      fromByteString "HTTP/1.1 103 Early Hints\r\n"
                      <> mconcat [renderHeader h | h <- headers]
                      <> "\r\n"
            }
    bracket
        (try $ do
           liftIO $ wpsLogRequest wps req'
           HC.responseOpen req' manager
        )
        (either (const $ return ()) HC.responseClose)
        $ \case
            Left (e :: SomeException) -> wpsOnExc wps e req sendToClient
            Right res -> do
                let res' = const () <$> res
                    conduit = fromMaybe
                                (awaitForever (\bs -> yield (Chunk $ fromByteString bs) >> yield Flush))
                                (wpsProcessBody wps req res')
                    src = bodyReaderSource $ HC.responseBody res
                    notEncoded = isNothing (lookup "content-encoding" (HC.responseHeaders res))
                    notChunked = HT.httpMajor (WAI.httpVersion req) >= 2 || WAI.requestMethod req == HT.methodHead

                let (HT.Status code message) = HC.responseStatus res
                sendToClient $ L.toStrict $ toLazyByteString $
                  fromByteString (S8.pack $ show (HC.responseVersion res)) <> " " <> fromByteString (S8.pack $ show code) <> " " <> fromByteString message <> "\r\n"

                let headers' = filter (\(key, v) -> not (key `Set.member` strippedHeaders) ||
                                                          key == "content-length" && ((notEncoded && notChunked) || v == "0"))
                               (wpsModifyResponseHeaders wps req res' (HC.responseHeaders res))
                let headers
                      | notChunked = headers'
                      | otherwise = (H.hTransferEncoding, "chunked") : headers'
                sendToClient $ L.toStrict $ toLazyByteString $
                    mconcat [renderHeader h | h <- headers]
                    <> "\r\n"

                -- It may look strange that we don't handle 'Flush' here, but 'Flush' is not actually used anywhere
                -- except at the end of the stream in the conduit above.
                let sendChunk
                      | notChunked = sendToClient . toByteString
                      | otherwise = sendToClient . toByteString . chunkedTransferEncoding
                runConduit $ src .| conduit .| CL.mapM_ (\mb ->
                    case mb of
                        Flush -> return ()
                        Chunk b -> sendChunk b
                    )
                unless notChunked $ sendToClient (toByteString chunkedTransferTerminator)

  where
    renderHeader :: HT.Header -> Builder
    renderHeader (name, value) = fromByteString (CI.original name) <> ": " <> fromByteString value <> "\r\n"


tryWebSockets :: (HasReadWrite ad) => WaiProxySettings -> (forall a. (ad -> IO a) -> IO a) -> WAI.Request -> (WAI.Response -> IO b) -> IO b -> IO b
tryWebSockets wps runConduitClient req sendResponse fallback
    | wpsUpgradeToRaw wps req =
        sendResponse $ flip WAI.responseRaw backup $ \fromClientBody toClient ->
            runConduitClient $ \server ->
                let toServer = DCN.appSink server
                    fromServer = DCN.appSource server
                    fromClient = do
                        mapM_ yield $ L.toChunks $ toLazyByteString headers
                        let loop = do
                                bs <- liftIO fromClientBody
                                unless (S.null bs) $ do
                                    yield bs
                                    loop
                        loop
                    toClient' = awaitForever $ liftIO . toClient
                    headers = renderHeaders $ fixReqHeaders wps req
                 in concurrently_
                        (runConduit $ fromClient .| toServer)
                        (runConduit $ fromServer .| toClient')
    | otherwise = fallback
  where
    backup = WAI.responseLBS HT.status500 [("Content-Type", "text/plain")]
        "http-reverse-proxy detected WebSockets request, but server does not support responseRaw"

    renderHeaders :: HT.RequestHeaders -> Builder
    renderHeaders headers
        = fromByteString (WAI.requestMethod req)
       <> fromByteString " "
       <> fromByteString (WAI.rawPathInfo req)
       <> fromByteString (WAI.rawQueryString req)
       <> (if WAI.httpVersion req == HT.http11
               then fromByteString " HTTP/1.1"
               else fromByteString " HTTP/1.0")
       <> mconcat (map goHeader headers)
       <> fromByteString "\r\n\r\n"
      where
        goHeader (x, y)
            = fromByteString "\r\n"
           <> fromByteString (CI.original x)
           <> fromByteString ": "
           <> fromByteString y


strippedHeaders :: Set HT.HeaderName
strippedHeaders = Set.fromList
    ["content-length", "transfer-encoding", "accept-encoding", "content-encoding"]

fixReqHeaders :: WaiProxySettings -> WAI.Request -> HT.RequestHeaders
fixReqHeaders wps req =
    addXRealIP $ filter (\(key, value) -> not $ key `Set.member` strippedHeaders
                                       || (key == "connection" && value == "close"))
               $ WAI.requestHeaders req
  where
    fromSocket = (("X-Real-IP", S8.pack $ showSockAddr $ WAI.remoteHost req):)
    fromForwardedFor = do
      h <- lookup "x-forwarded-for" (WAI.requestHeaders req)
      listToMaybe $ map (TE.encodeUtf8 . T.strip) $ T.splitOn "," $ TE.decodeUtf8 h
    addXRealIP =
        case wpsSetIpHeader wps of
            SIHFromSocket -> fromSocket
            SIHFromHeader ->
                case lookup "x-real-ip" (WAI.requestHeaders req) <|> fromForwardedFor of
                    Nothing -> fromSocket
                    Just ip -> (("X-Real-IP", ip):)
            SIHNone -> id




{- FIXME
-- | Convert a WAI application into a raw application, using Warp.
waiToRaw :: WAI.Application -> DCN.Application IO
waiToRaw app appdata0 =
    loop fromClient0
  where
    fromClient0 = DCN.appSource appdata0
    toClient = DCN.appSink appdata0
    loop fromClient = do
        mfromClient <- runResourceT $ withInternalState $ \internalState -> do
            ex <- try $ parseRequest conn internalState dummyAddr fromClient
            case ex of
                Left (_ :: SomeException) -> return Nothing
                Right (req, fromClient') -> do
                    res <- app req
                    keepAlive <- sendResponse
                        defaultSettings
                        req conn res
                    (fromClient'', _) <- liftIO fromClient' >>= unwrapResumable
                    return $ if keepAlive then Just fromClient'' else Nothing
        maybe (return ()) loop mfromClient

    dummyAddr = SockAddrInet (PortNum 0) 0 -- FIXME
    conn = Connection
        { connSendMany = \bss -> mapM_ yield bss $$ toClient
        , connSendAll = \bs -> yield bs $$ toClient
        , connSendFile = \fp offset len _th headers _cleaner ->
            let src1 = mapM_ yield headers
                src2 = sourceFileRange fp (Just offset) (Just len)
             in runResourceT
                $  (src1 >> src2)
                $$ transPipe lift toClient
        , connClose = return ()
        , connRecv = error "connRecv should not be used"
        }
        -}

bodyReaderSource :: MonadIO m => BodyReader -> ConduitT i ByteString m ()
bodyReaderSource br =
    loop
  where
    loop = do
        bs <- liftIO $ brRead br
        unless (S.null bs) $ do
            yield bs
            loop
