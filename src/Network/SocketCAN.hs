{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.SocketCAN
  ( withSocketCAN
  , sendCANMessage
  , recvCANMessage
  , Network.Socket.ifNameToIndex
  ) where

import Network.CAN (CANMessage)
import Network.Socket (Socket)
import Network.SocketCAN.Bindings (SockAddrCAN(..))

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
