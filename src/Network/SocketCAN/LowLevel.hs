{-# LANGUAGE TypeApplications #-}
module Network.SocketCAN.LowLevel
  ( socket
  , bind
  , send
  , recv
  , module Network.Socket
  ) where

import Control.Monad (void)
import Foreign.Ptr (Ptr)
import Network.Socket (Family(AF_CAN), Socket, close)
import Network.SocketCAN.Bindings (SockAddrCAN(..), SocketCANFrame)

import qualified Foreign.Ptr
import qualified Foreign.Marshal.Alloc
import qualified Foreign.Storable
import qualified Network.Socket
import qualified Network.Socket.Address
import qualified Network.SocketCAN.Bindings

-- | Create raw CAN socket
socket
  :: IO Socket
socket =
  Network.Socket.socket
    AF_CAN
    Network.Socket.Raw
    Network.SocketCAN.Bindings.CAN_RAW

-- | Bind CAN socket
bind
  :: Socket
  -> SockAddrCAN
  -> IO ()
bind = Network.Socket.Address.bind

send
  :: Socket
  -> SocketCANFrame
  -> IO ()
send canSock cf =
  Foreign.Marshal.Alloc.alloca $ \ptr -> do
    Foreign.Storable.poke ptr cf
    void
      $ Network.Socket.sendBuf
          canSock
          (Foreign.Ptr.castPtr ptr)
          (Foreign.Storable.sizeOf cf)

recv
  :: Socket
  -> IO SocketCANFrame
recv canSock =
  Foreign.Marshal.Alloc.alloca $ \ptr -> do
    (_nBytes, _sockAddr) <-
      Network.Socket.Address.recvBufFrom
        @SockAddrCAN
        canSock
        (ptr :: Ptr SocketCANFrame)
        (Foreign.Storable.sizeOf ptr)
    Foreign.Storable.peek ptr >>= pure
