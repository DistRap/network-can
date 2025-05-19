{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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
  , SLCANT(..)
  , runSLCAN
  ) where

import Control.Exception (Exception)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT(..))

import Network.Socket (Socket, SockAddr)
import Network.CAN (CANMessage, MonadCAN(..))
import Network.SLCAN.Types
import System.IO (Handle)
import UnliftIO (MonadUnliftIO)

import qualified Control.Monad
import qualified Control.Exception
import qualified Data.ByteString
import qualified Data.ByteString.Char8
import qualified System.IO
import qualified Network.SLCAN.Builder
import qualified Network.SLCAN.Parser
import qualified Network.Socket.ByteString
import qualified UnliftIO

data Transport =
    Transport_Handle Handle
  | Transport_UDP Socket SockAddr

withSLCANTransport
  :: Transport
  -> SLCANConfig
  -> (Transport -> IO a)
  -> IO a
withSLCANTransport transport SLCANConfig{..} act = do
  let sendC = sendSLCANControl transport
  Control.Exception.finally
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
  :: Transport
  -> SLCANMessage
  -> IO ()
sendSLCANMessage (Transport_Handle handle) msg = do
  Control.Monad.void
    $ Data.ByteString.hPutStr
        handle
        $ Network.SLCAN.Builder.buildSLCANMessage
            msg
  System.IO.hFlush handle
sendSLCANMessage (Transport_UDP socket target) msg = do
  Network.Socket.ByteString.sendAllTo
    socket
    (Network.SLCAN.Builder.buildSLCANMessage msg)
    target

sendSLCANControl
  :: Transport
  -> SLCANControl
  -> IO ()
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

newtype SLCANT m a = SLCANT
  { _unSLCANT :: ReaderT Transport m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Transport
    , MonadIO
    , MonadUnliftIO
    )

instance MonadTrans SLCANT where
  lift = SLCANT . lift

-- | Run SLCANT transformer
runSLCANT
  :: Monad m
  => Transport
  -> SLCANT m a
  -> m a
runSLCANT t =
    (`runReaderT` t)
  . _unSLCANT

data SLCANException = SLCANException_ParseError String
    deriving Show

instance Exception SLCANException

runSLCAN
  :: ( MonadIO m
     , MonadUnliftIO m
     )
  => Transport
  -> SLCANConfig
  -> SLCANT m a
  -> m a
runSLCAN transport config act = do
  UnliftIO.withRunInIO $ \runInIO ->
    withSLCANTransport
      transport
      config
      (\t -> runInIO (runSLCANT t act))

instance MonadIO m => MonadCAN (SLCANT m) where
  send cm = do
    ask >>= liftIO . flip sendCANMessage cm
  recv = do
    transport <- ask
    liftIO
      (recvSLCANMessage transport)
    >>= \case
      Left e ->
        UnliftIO.throwIO $ SLCANException_ParseError e
      Right (SLCANMessage_Data cm) ->
        pure cm
      Right _other ->
        -- TODO: do something with
        -- SLCANMessage_Error
        -- and SLCANMessage_State
        -- like allow registering handlers for these
        -- or throwIO on _Error one
        recv
