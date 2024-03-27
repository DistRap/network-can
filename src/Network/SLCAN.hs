{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Network.SLCAN
  ( SLCANT
  , runSLCANT
  , runSLCAN
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
import System.Hardware.Serialport (SerialPort, SerialPortSettings)

import qualified Control.Monad.Catch
import qualified System.Hardware.Serialport
import qualified Network.SLCAN.Builder
import qualified Network.SLCAN.Parser

newtype SLCANT m a = SLCANT
  { _unSLCANT
      :: ExceptT CANError
          (ReaderT SerialPort m) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader SerialPort
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
  => SerialPort
  -> SLCANT m a
  -> m (Either CANError a)
runSLCANT sp =
    (`runReaderT` sp)
  . runExceptT
  . _unSLCANT

runSLCAN
  :: ( MonadIO m
     , MonadMask m
     )
  => FilePath
  -> SerialPortSettings
  -> SLCANT m a
  -> m (Either CANError a)
runSLCAN fp settings act = do
  Control.Monad.Catch.bracket
    (liftIO
     $ System.Hardware.Serialport.openSerial
         fp
         settings
    )
    (liftIO . System.Hardware.Serialport.closeSerial)
    (\sp -> runSLCANT sp act)

instance MonadIO m => MonadCAN (SLCANT m) where
  send cm = do
    sp <- ask
    void
      $ liftIO
      $ System.Hardware.Serialport.send
          sp
          (Network.SLCAN.Builder.buildSLCANMessage cm)

  recv = do
    sp <- ask
    raw <-
      liftIO
      $ System.Hardware.Serialport.recv
          sp
          1024

    case Network.SLCAN.Parser.parseSLCANMessage raw of
      Left e -> throwError $ CANError_SLCANParseError e
      Right x -> pure x
