{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Network.SLCAN
  ( SLCANT
  , runSLCANT
  , runSLCAN
  ) where

import Control.Monad (void)
import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)

import Data.ByteString (ByteString)

import Network.CAN.Monad (CANError(..), MonadCAN(..))
import Network.CAN.Types (CANArbitrationField(..), CANMessage(..))
import System.Hardware.Serialport (SerialPort, SerialPortSettings)

import qualified Control.Monad.Catch
import qualified Data.Bool
import qualified System.Hardware.Serialport

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
          (toSLCANFrame cm)
  recv = do
    sp <- ask
    liftIO
      $ System.Hardware.Serialport.recv
          sp
          1024
      >>= pure . fromSLCANFrame

-- <type> <id> <dlc> <data>

toSLCANFrame
  :: CANMessage
  -> ByteString
toSLCANFrame CANMessage{..} =
  mempty
--  SLCANFrame
--  { socketCANFrameArbitrationField =
--      toSLCANArbitrationField
--        canMessageArbitrationField
--  , socketCANFrameLength =
--      fromIntegral $ length canMessageData
--  , socketCANFrameData = canMessageData
--  }

fromSLCANFrame
  :: ByteString
  -> CANMessage
fromSLCANFrame = undefined
{--
  CANMessage
  { canMessageArbitrationField =
      fromSLCANArbitrationField
        socketCANFrameArbitrationField
  , canMessageData = socketCANFrameData
  }

toSLCANArbitrationField
  :: CANArbitrationField
  -> SLCANArbitrationField
toSLCANArbitrationField CANArbitrationField{..} =
  SLCANArbitrationField
  $ Data.Bool.bool
      id
      (.|. effBit)
      canArbitrationFieldExtended
  $ Data.Bool.bool
      id
      (.|. rtrBit)
      canArbitrationFieldRTR
  $ canArbitrationFieldID
fromSLCANArbitrationField
  :: SLCANArbitrationField
  -> CANArbitrationField
fromSLCANArbitrationField (SLCANArbitrationField scid) =
  let
    isEff = scid .&. effBit /= 0
  in
    CANArbitrationField
    { canArbitrationFieldID =
        Data.Bool.bool
          (.&. (1 `shiftL` 12 - 1))
          (.&. (1 `shiftL` 30 - 1))
          isEff
        $ scid
    , canArbitrationFieldExtended = isEff
    , canArbitrationFieldRTR = scid .&. rtrBit /= 0
    }

effBit :: Word32
effBit = 1 `shiftL` 31

rtrBit :: Word32
rtrBit = 1 `shiftL` 30
--}
