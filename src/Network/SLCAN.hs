{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Network.SLCAN
  ( withSLCANHandle
  , sendSLCANMessage
  , sendSLCANControl
  , recvSLCANMessage
  , sendCANMessage
  ) where

import Network.CAN.Types (CANMessage)
import Network.SLCAN.Types
  ( SLCANMessage(..)
  , SLCANControl(..)
  , SLCANConfig(..)
  )
import System.IO (Handle)

import qualified Control.Monad
import qualified Control.Exception
import qualified Data.ByteString
import qualified Data.ByteString.Char8
import qualified System.IO
import qualified Network.SLCAN.Builder
import qualified Network.SLCAN.Parser

withSLCANHandle
  :: Handle
  -> SLCANConfig
  -> (Handle -> IO a)
  -> IO a
withSLCANHandle handle SLCANConfig{..} act = do
  let send = sendSLCANControl handle
  Control.Exception.finally
    (do
       send SLCANControl_Close
       send (SLCANControl_Bitrate slCANConfigBitrate)
       Control.Monad.when
         slCANConfigResetErrors
         (send SLCANControl_ResetErrors)
       send
         (if slCANConfigListenOnly
          then SLCANControl_ListenOnly
          else SLCANControl_Open
         )
       Control.Exception.finally
         (act handle)
         (send SLCANControl_Close)
    )
    (System.IO.hClose handle)

{--
runSLCANFilePath
  :: ( MonadIO m
     , MonadMask m
     )
  => FilePath
  -> SLCANConfig
  -> SLCANT m a
  -> m (Either CANError a)
runSLCANFilePath fp conf act = do
  h <-
    liftIO
    $ System.IO.openFile
        fp
        System.IO.ReadWriteMode
  runSLCANHandle h conf act

runSLCANSerialPort
  :: ( MonadIO m
     , MonadMask m
     )
  => FilePath
  -> SerialPortSettings
  -> SLCANConfig
  -> SLCANT m a
  -> m (Either CANError a)
runSLCANSerialPort fp sportSettings conf act = do
  h <-
     liftIO
  runSLCANHandle h conf act
--}

sendSLCANMessage
  :: Handle
  -> SLCANMessage
  -> IO ()
sendSLCANMessage handle msg = do
  Control.Monad.void
    $ Data.ByteString.hPutStr
        handle
        $ Network.SLCAN.Builder.buildSLCANMessage
            msg
  System.IO.hFlush handle

sendSLCANControl
  :: Handle
  -> SLCANControl
  -> IO ()
sendSLCANControl h =
  sendSLCANMessage h
  . SLCANMessage_Control

recvSLCANMessage
  :: Handle
  -> IO (Either String SLCANMessage)
recvSLCANMessage handle = do
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

sendCANMessage
  :: Handle
  -> CANMessage
  -> IO ()
sendCANMessage h =
  sendSLCANMessage h
  . SLCANMessage_Data
