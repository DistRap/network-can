module Network.SocketCAN
  ( withSocket
  , sendCANMessage
  , recvCANMessage
  , Network.Socket.ifNameToIndex
  , CANInterface
  , mkCANInterface
  , NoSuchInterface(..)
  , withSocketCAN
  ) where

import Network.CAN (CANMessage, CAN(..))
import Network.Socket (Socket)
import Network.SocketCAN.Bindings (SockAddrCAN(..))

import Control.Monad.Class.MonadThrow (Exception(..), MonadThrow(bracket, throwIO))
import Control.Monad.IO.Class (MonadIO(..))

import qualified Network.Socket (ifNameToIndex)
import qualified Network.SocketCAN.LowLevel
import qualified Network.SocketCAN.Translate

withSocket
  :: ( MonadIO m
     , MonadThrow m
     )
  => Int
  -> (Socket -> m a)
  -> m a
withSocket ifaceIdx act = do
  bracket
    (liftIO Network.SocketCAN.LowLevel.socket)
    (liftIO . Network.SocketCAN.LowLevel.close)
    (\canSock -> do
      liftIO
        $ Network.SocketCAN.LowLevel.bind
            canSock
            $ Network.SocketCAN.Bindings.SockAddrCAN
              $ fromIntegral ifaceIdx
      act canSock
    )

sendCANMessage
  :: Socket
  -> CANMessage
  -> IO ()
sendCANMessage canSock cm =
  Network.SocketCAN.LowLevel.send
    canSock
    (Network.SocketCAN.Translate.toSocketCANFrame cm)

recvCANMessage
  :: Socket
  -> IO CANMessage
recvCANMessage canSock =
  Network.SocketCAN.LowLevel.recv canSock
  >>= pure . Network.SocketCAN.Translate.fromSocketCANFrame

newtype CANInterface = CANInterface
  { unCANInterface :: String }
  deriving Eq

instance Show CANInterface where
  show = unCANInterface

mkCANInterface :: String -> CANInterface
mkCANInterface = CANInterface

data NoSuchInterface = NoSuchInterface
    deriving Show

instance Exception NoSuchInterface

withSocketCAN
  :: ( MonadIO m
     , MonadThrow m
     )
  => CANInterface
  -> (CAN m -> m a)
  -> m a
withSocketCAN interface act = do
  mIdx <-
    liftIO
    $ Network.Socket.ifNameToIndex (unCANInterface interface)

  case mIdx of
    Nothing -> throwIO NoSuchInterface
    Just idx ->
      withSocket
        idx
        $ \sock ->
            act
              CAN
                { canSend = liftIO . sendCANMessage sock
                , canRecv = liftIO $ recvCANMessage sock
                }
