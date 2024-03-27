{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards #-}
module Network.SocketCAN
  ( SocketCANT
  , runSocketCANT
  , runSocketCAN
  ) where

import Control.Monad.Catch (MonadCatch, MonadMask, MonadThrow)
import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Control.Monad.Trans.Reader (ReaderT, runReaderT)

import Data.Bits ((.&.), (.|.), shiftL)
import Data.Word (Word32)

import Network.CAN.Monad (CANError(..), MonadCAN(..))
import Network.CAN.Types (CANArbitrationField(..), CANMessage(..))
import Network.Socket (Socket)
import Network.SocketCAN.Bindings (SockAddrCAN(..), SocketCANArbitrationField(..), SocketCANFrame(..))

import qualified Control.Monad.Catch
import qualified Data.Bool
import qualified Network.Socket
import qualified Network.SocketCAN.LowLevel

newtype SocketCANT m a = SocketCANT
  { _unSocketCANT
      :: ExceptT CANError
          (ReaderT Socket m) a
  }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Socket
    , MonadError CANError
    , MonadCatch
    , MonadMask
    , MonadThrow
    , MonadIO
    )

instance MonadTrans SocketCANT where
  lift = SocketCANT . lift . lift

-- | Run SocketCANT transformer
runSocketCANT
  :: Monad m
  => Socket
  -> SocketCANT m a
  -> m (Either CANError a)
runSocketCANT sock =
    (`runReaderT` sock)
  . runExceptT
  . _unSocketCANT

runSocketCAN
  :: ( MonadIO m
     , MonadMask m
     )
  => String
  -> SocketCANT m a
  -> m (Either CANError a)
runSocketCAN interface act = do
  mIdx <-
    liftIO
    $ Network.Socket.ifNameToIndex interface

  case mIdx of
    Nothing ->
      pure
      $ Left
      $ CANError_NoSuchInterface interface

    Just idx -> do
      Control.Monad.Catch.bracket
        (liftIO Network.SocketCAN.LowLevel.socket)
        (liftIO . Network.SocketCAN.LowLevel.close)
        (\canSock -> do
          liftIO
            $ Network.SocketCAN.LowLevel.bind
                canSock
                $ Network.SocketCAN.Bindings.SockAddrCAN
                  $ fromIntegral idx
          runSocketCANT canSock act
        )

instance MonadIO m => MonadCAN (SocketCANT m) where
  send cm = do
    canSock <- ask
    liftIO
      $ Network.SocketCAN.LowLevel.send
          canSock
          (toSocketCANFrame cm)
  recv = do
    canSock <- ask
    liftIO
      $ Network.SocketCAN.LowLevel.recv canSock
      >>= pure . fromSocketCANFrame

toSocketCANFrame
  :: CANMessage
  -> SocketCANFrame
toSocketCANFrame CANMessage{..} =
  SocketCANFrame
  { socketCANFrameArbitrationField =
      toSocketCANArbitrationField
        canMessageArbitrationField
  , socketCANFrameLength =
      fromIntegral $ length canMessageData
  , socketCANFrameData = canMessageData
  }

fromSocketCANFrame
  :: SocketCANFrame
  -> CANMessage
fromSocketCANFrame SocketCANFrame{..} =
  CANMessage
  { canMessageArbitrationField =
      fromSocketCANArbitrationField
        socketCANFrameArbitrationField
  , canMessageData = socketCANFrameData
  }

toSocketCANArbitrationField
  :: CANArbitrationField
  -> SocketCANArbitrationField
toSocketCANArbitrationField CANArbitrationField{..} =
  SocketCANArbitrationField
  $ Data.Bool.bool
      id
      (.|. effBit)
      canArbitrationFieldExtended
  $ Data.Bool.bool
      id
      (.|. rtrBit)
      canArbitrationFieldRTR
  $ canArbitrationFieldID
fromSocketCANArbitrationField
  :: SocketCANArbitrationField
  -> CANArbitrationField
fromSocketCANArbitrationField (SocketCANArbitrationField scid) =
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
