{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.SocketCAN
  ( withSocketCAN
  , sendCANMessage
  , recvCANMessage
  , Network.Socket.ifNameToIndex
  , runSocketCAN
  ) where

import Network.CAN (CANMessage, MonadCAN(..))
import Network.Socket (Socket)
import Network.SocketCAN.Bindings (SockAddrCAN(..))

import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT(..))
import UnliftIO

import qualified Control.Exception
import qualified Network.Socket (ifNameToIndex)
import qualified Network.SocketCAN.LowLevel
import qualified Network.SocketCAN.Translate

withSocketCAN
  :: Int
  -> (Socket -> IO a)
  -> IO a
withSocketCAN ifaceIdx act = do
  Control.Exception.bracket
    Network.SocketCAN.LowLevel.socket
    Network.SocketCAN.LowLevel.close
    (\canSock -> do
      Network.SocketCAN.LowLevel.bind
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

newtype SocketCANT m a = SocketCANT
  { _unSocketCANT :: ReaderT Socket m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Socket
    , MonadIO
    , MonadUnliftIO
    )

instance MonadTrans SocketCANT where
  lift = SocketCANT . lift

-- | Run SocketCANT transformer
runSocketCANT
  :: Monad m
  => Socket
  -> SocketCANT m a
  -> m a
runSocketCANT sock =
    (`runReaderT` sock)
  . _unSocketCANT

data NoSuchInterface = NoSuchInterface
    deriving Show

instance Exception NoSuchInterface

runSocketCAN
  :: ( MonadIO m
     , MonadUnliftIO m
     )
  => String
  -> SocketCANT m a
  -> m a
runSocketCAN interface act = do
  mIdx <-
    liftIO
    $ Network.Socket.ifNameToIndex interface

  case mIdx of
    Nothing -> throwIO NoSuchInterface
    Just idx -> withRunInIO $ \runInIO ->
      withSocketCAN idx (\s -> runInIO (runSocketCANT s act))

instance MonadIO m => MonadCAN (SocketCANT m) where
  send cm = do
    canSock <- ask
    liftIO $ sendCANMessage canSock cm
  recv = do
    canSock <- ask
    liftIO $ recvCANMessage canSock
