{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE RankNTypes  #-}

module Network.HTTP2.Client.RawConnection (
      RawHttp2Connection (..)
    , newRawHttp2Connection
    ) where

import           Control.Monad (forever, when)
import           Control.Concurrent.Async (Async, async, cancel, pollSTM)
import           Control.Concurrent.STM (STM, atomically, retry, throwSTM)
import           Control.Concurrent.STM.TVar (TVar, modifyTVar', newTVarIO, readTVar, writeTVar)
import           Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import           Data.ByteString.Lazy (fromChunks)
import           Data.Foldable (fold)
import           Data.Monoid ((<>))
import qualified Network.Connection as Connection
import qualified Network.HTTP2 as HTTP2
import           Network.Socket hiding (recv)
import           Network.Socket.ByteString
import qualified Network.TLS as TLS

-- TODO: catch connection errrors
data RawHttp2Connection = RawHttp2Connection {
    _sendRaw :: [ByteString] -> IO ()
  -- ^ Function to send raw data to the server.
  , _nextRaw :: Int -> IO ByteString
  -- ^ Function to block reading a datachunk of a given size from the server.
  , _close   :: IO ()
  }

-- | Initiates a RawHttp2Connection with a server.
--
-- The current code does not handle closing the connexion, yikes.
newRawHttp2Connection :: HostName
                      -- ^ Server's hostname.
                      -> PortNumber
                      -- ^ Server's port to connect to.
                      -> Maybe TLS.ClientParams
                      -- ^ TLS parameters. The 'TLS.onSuggestALPN' hook is
                      -- overwritten to always return ["h2", "h2-17"].
                      -> IO RawHttp2Connection
newRawHttp2Connection host port mparams = do
    ctx <- Connection.initConnectionContext
    -- Connects to TCP.
    let hints = defaultHints { addrFlags = [AI_NUMERICSERV], addrSocketType = Stream }
        params = Connection.ConnectionParams host port (Connection.TLSSettings <$> mparams) Nothing
    addr:_ <- getAddrInfo (Just hints) (Just host) (Just $ show port)
    skt <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    setSocketOption skt NoDelay 1
    connect skt (addrAddress addr)
    conn <- Connection.connectFromSocket ctx skt params
    pure $ RawHttp2Connection
        (Connection.connectionPut conn . fold)
        (Connection.connectionGetExact conn)
        (Connection.connectionClose conn)
