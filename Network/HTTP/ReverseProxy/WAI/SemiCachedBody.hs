{-# LANGUAGE LambdaCase            #-}

module Network.HTTP.ReverseProxy.WAI.SemiCachedBody (
  semiCachedBody
  ) where

import           Data.ByteString                      (ByteString)
import qualified Data.ByteString                      as S
import           Data.IORef
import           Data.List.NonEmpty                   (NonEmpty (..))
import qualified Data.List.NonEmpty                   as NE
import qualified Network.HTTP.Client                  as HC


-- | Introduce a minor level of caching to handle some basic
-- retry cases inside http-client. But to avoid a DoS attack,
-- don't cache more than 65535 bytes (the theoretical max TCP packet size).
--
-- See: <https://github.com/fpco/http-reverse-proxy/issues/34#issuecomment-719136064>
semiCachedBody :: IO ByteString -> IO (HC.GivesPopper ())
semiCachedBody orig = do
  ref <- newIORef $ SCBCaching 0 []
  pure $ \needsPopper -> do
    let fromChunks len chunks =
          case NE.nonEmpty (reverse chunks) of
            Nothing -> SCBCaching len chunks
            Just toDrain -> SCBDraining len chunks toDrain
    state0 <- readIORef ref >>=
      \case
        SCBCaching len chunks -> pure $ fromChunks len chunks
        SCBDraining len chunks _ -> pure $ fromChunks len chunks
        SCBTooMuchData -> error "Cannot retry this request body, need to force a new request"
    writeIORef ref $! state0
    let popper :: IO ByteString
        popper = do
          readIORef ref >>=
            \case
              SCBDraining len chunks (next:|rest) -> do
                writeIORef ref $!
                  case rest of
                    [] -> SCBCaching len chunks
                    x:xs -> SCBDraining len chunks (x:|xs)
                pure next
              SCBTooMuchData -> orig
              SCBCaching len chunks -> do
                bs <- orig
                let newLen = len + S.length bs
                writeIORef ref $!
                  if newLen > maxCache
                    then SCBTooMuchData
                    else SCBCaching newLen (bs:chunks)
                pure bs

    needsPopper popper
  where
    maxCache = 65535

data SCB
  = SCBCaching !Int ![ByteString]
  | SCBDraining !Int ![ByteString] !(NonEmpty ByteString)
  | SCBTooMuchData
