{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecordWildCards #-}
module Network.SLCAN
  ( SLCANT
  , runSLCANT
  , runSLCANFilePath
  , runSLCANSerialPort
  ) where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (MonadError, throwError)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)

import Network.CAN.Monad (CANError(..), MonadCAN(..))
import Network.SLCAN.Types
  ( SLCANMessage(..)
  , SLCANControl(..)
  , SLCANConfig(..)
  )
import System.IO (Handle)
import System.Hardware.Serialport (SerialPortSettings)

import qualified Control.Monad
import qualified Control.Monad.Catch
import qualified Data.ByteString
import qualified Data.ByteString.Char8
import qualified System.Hardware.Serialport
import qualified System.IO
import qualified Network.SLCAN.Builder
import qualified Network.SLCAN.Parser

newtype SLCANT m a = SLCANT
  { _unSLCANT
      :: ExceptT CANError
          (ReaderT Handle m) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Handle
    , MonadError CANError
    , MonadCatch
    , MonadMask
    , MonadThrow
    , MonadIO
    )

instance MonadTrans SLCANT where
  lift = SLCANT . lift . lift

-- | Run SLCANT transformer
runSLCANT
  :: Monad m
  => Handle
  -> SLCANT m a
  -> m (Either CANError a)
runSLCANT handle =
    (`runReaderT` handle)
  . runExceptT
  . _unSLCANT

runSLCANHandle
  :: ( MonadIO m
     , MonadMask m
     )
  => Handle
  -> SLCANConfig
  -> SLCANT m a
  -> m (Either CANError a)
runSLCANHandle handle conf act =
  Control.Monad.Catch.finally
    (runSLCANT handle (openClose act conf))
    (liftIO $ System.IO.hClose handle)
  where
    openClose origAct SLCANConfig{..} = do
      sendSLCANControl
        SLCANControl_Close
      sendSLCANControl
        (SLCANControl_Bitrate slCANConfigBitrate)
      Control.Monad.when
        slCANConfigResetErrors
        (sendSLCANControl SLCANControl_ResetErrors)
      sendSLCANControl
        (if slCANConfigListenOnly
         then SLCANControl_ListenOnly
         else SLCANControl_Open
        )
      Control.Monad.Catch.finally
        origAct
        (sendSLCAN (SLCANMessage_Control SLCANControl_Close))

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
     $ System.Hardware.Serialport.hOpenSerial
         fp
         sportSettings

  runSLCANHandle h conf act

sendSLCAN
  :: ( MonadIO m
     , MonadReader Handle m
     )
  => SLCANMessage
  -> m ()
sendSLCAN msg = do
  handle <- ask
  Control.Monad.void
    $ liftIO
    $ Data.ByteString.hPutStr
        handle
        $ Network.SLCAN.Builder.buildSLCANMessage
            msg

sendSLCANControl
  :: ( MonadIO m
     , MonadReader Handle m
     )
  => SLCANControl
  -> m ()
sendSLCANControl = sendSLCAN . SLCANMessage_Control

instance MonadIO m => MonadCAN (SLCANT m) where
  send = sendSLCAN . SLCANMessage_Data

  recv = do
    handle <- ask
    raw <- hGetTillCR handle

    case Network.SLCAN.Parser.parseSLCANMessage raw of
      Left e -> throwError $ CANError_SLCANParseError e
      Right (SLCANMessage_Data cmsg) -> pure cmsg
      -- XXX: we can only get SLCANMessage_State
      -- or SLCANMessage_Error
      -- we should do something about the errors
      -- or i.e. Passive/BusOff state
      Right _ -> recv

    where
      hGetTillCR handle = do
        msg <-
          liftIO
          $ Data.ByteString.hGetSome
              handle
              1024
        if Data.ByteString.Char8.last msg == '\r'
        then pure msg
        else hGetTillCR handle >>= pure . (msg <>)
