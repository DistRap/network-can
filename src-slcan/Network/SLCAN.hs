{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Network.SLCAN
  ( Transport(..)
  , withSLCANTransport
  , sendSLCANMessage
  , sendSLCANControl
  , recvSLCANMessage
  , sendCANMessage
  , module Network.SLCAN.Types
  , SLCANException(..)
  , withSLCAN
  ) where

import Control.Monad.Class.MonadThrow (Exception(..), MonadThrow(throwIO), finally)
import Control.Monad.IO.Class (MonadIO(..))

import Network.Socket (Socket, SockAddr)
import Network.CAN (CANMessage, CANEndpoint(..))
import Network.SLCAN.Types
import System.IO (Handle)

import qualified Control.Monad
import qualified Data.ByteString
import qualified Data.ByteString.Char8
import qualified System.IO
import qualified Network.SLCAN.Builder
import qualified Network.SLCAN.Parser
import qualified Network.Socket.ByteString

data Transport =
    Transport_Handle Handle
  | Transport_UDP Socket SockAddr

withSLCANTransport
  :: ( MonadIO m
     , MonadThrow m
     )
  => Transport
  -> SLCANConfig
  -> (Transport -> m a)
  -> m a
withSLCANTransport transport SLCANConfig{..} act = do
  let sendC = sendSLCANControl transport
  finally
    (do
       sendC SLCANControl_Close
       sendC (SLCANControl_Bitrate slCANConfigBitrate)
       Control.Monad.when
         slCANConfigResetErrors
         (sendC SLCANControl_ResetErrors)
       sendC
         (if slCANConfigListenOnly
          then SLCANControl_ListenOnly
          else SLCANControl_Open
         )

       act transport
    )
    (sendC SLCANControl_Close)

sendSLCANMessage
  :: MonadIO m
  => Transport
  -> SLCANMessage
  -> m ()
sendSLCANMessage (Transport_Handle handle) msg = liftIO $ do
  Control.Monad.void
    $ Data.ByteString.hPutStr
        handle
        $ Network.SLCAN.Builder.buildSLCANMessage
            msg
  System.IO.hFlush handle
sendSLCANMessage (Transport_UDP socket target) msg =
  liftIO
  $ Network.Socket.ByteString.sendAllTo
      socket
      (Network.SLCAN.Builder.buildSLCANMessage msg)
      target

sendSLCANControl
  :: MonadIO m
  => Transport
  -> SLCANControl
  -> m ()
sendSLCANControl t =
  sendSLCANMessage t
  . SLCANMessage_Control

recvSLCANMessage
  :: Transport
  -> IO (Either String SLCANMessage)
recvSLCANMessage (Transport_Handle handle) = do
  Network.SLCAN.Parser.parseSLCANMessage
  <$> hGetTillCR handle

  where
    hGetTillCR h = do
      msg <-
        Data.ByteString.hGetSome
          h
          1024
      if Data.ByteString.Char8.last msg == '\r'
      then pure msg
      else hGetTillCR h >>= pure . (msg <>)

recvSLCANMessage (Transport_UDP socket _target) = do
  Network.SLCAN.Parser.parseSLCANMessage
  <$> sockGetTillCR socket
  where
    sockGetTillCR s = do
      (msg, _source) <-
        Network.Socket.ByteString.recvFrom
          s
          1024
      if Data.ByteString.Char8.last msg == '\r'
      then pure msg
      else sockGetTillCR s >>= pure . (msg <>)

sendCANMessage
  :: Transport
  -> CANMessage
  -> IO ()
sendCANMessage t =
  sendSLCANMessage t
  . SLCANMessage_Data

data SLCANException = SLCANException_ParseError String
    deriving Show

instance Exception SLCANException

withSLCAN
  :: ( MonadIO m
     , MonadThrow m
     )
  => Transport
  -> SLCANConfig
  -> (CANEndpoint m -> m a)
  -> m a
withSLCAN transport config act = do
  withSLCANTransport
    transport
    config
    $ \t ->
        act
          CANEndpoint
            { canEndpointSend = liftIO . sendCANMessage t
            , canEndpointRecv =
                let
                  recv =
                    liftIO
                      (recvSLCANMessage t)
                    >>= \case
                      Left e ->
                        throwIO $ SLCANException_ParseError e
                      Right (SLCANMessage_Data cm) ->
                        pure cm
                      Right _other ->
                        -- TODO: do something with
                        -- SLCANMessage_Error
                        -- and SLCANMessage_State
                        -- like allow registering handlers for these
                        -- or throwIO on _Error one
                        recv
                in recv
            }
