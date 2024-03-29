{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Network.SLCAN
  ( SLCANT
  , runSLCANT
  , runSLCANFilePath
  , runSLCANSerialPort
  ) where

import Control.Monad (void)
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
--  , SLCANControl(..)
--  , SLCANBitrate(..)
  )
import System.IO (Handle)
import System.Hardware.Serialport (SerialPortSettings)

import qualified Control.Monad.Catch
import qualified Data.ByteString
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

runSLCANFilePath
  :: ( MonadIO m
     , MonadMask m
     )
  => FilePath
  -> SLCANT m a
  -> m (Either CANError a)
runSLCANFilePath fp act = do
  Control.Monad.Catch.bracket
    (liftIO
     $ System.IO.openFile
         fp
         System.IO.ReadWriteMode
    )
    (liftIO . System.IO.hClose)
    (\handle -> runSLCANT handle act)

runSLCANSerialPort
  :: ( MonadIO m
     , MonadMask m
     )
  => FilePath
  -> SerialPortSettings
  -> SLCANT m a
  -> m (Either CANError a)
runSLCANSerialPort fp settings act = do
  Control.Monad.Catch.bracket
    (liftIO
     $ System.Hardware.Serialport.hOpenSerial
         fp
         settings
    )
    (liftIO . System.IO.hClose)
    (\handle -> runSLCANT handle act)

instance MonadIO m => MonadCAN (SLCANT m) where
  send cm = do
    handle <- ask
    void
      $ liftIO
      $ Data.ByteString.hPutStr
          handle
          (Network.SLCAN.Builder.buildSLCANMessage
             $ SLCANMessage_Data cm
          )

  recv = do
    handle <- ask
    raw <-
      liftIO
      $ Data.ByteString.hGet
          handle
          1024

    case Network.SLCAN.Parser.parseSLCANMessage raw of
      Left e -> throwError $ CANError_SLCANParseError e
      Right (SLCANMessage_Data cmsg) -> pure cmsg
      -- XXX: we can only get SLCANMessage_State
      -- or SLCANMessage_Error, while the state
      -- is safe to ignore, we should do something about the errors
      Right _ -> recv
