{-# LANGUAGE GeneralizedNewtypeDeriving #-}
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

import Network.CAN.Monad (CANError(..), MonadCAN(..))
import Network.Socket (Socket)
import Network.SocketCAN.Bindings (SockAddrCAN(..))

import qualified Control.Monad.Catch
import qualified Network.Socket
import qualified Network.SocketCAN.LowLevel
import qualified Network.SocketCAN.Translate

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
          (Network.SocketCAN.Translate.toSocketCANFrame cm)
  recv = do
    canSock <- ask
    liftIO
      $ Network.SocketCAN.LowLevel.recv canSock
      >>= pure . Network.SocketCAN.Translate.fromSocketCANFrame
