module Network.CAN.Test where

import Control.Monad (forever)
import Network.CAN
import Network.Socket (Socket)
import Network.SocketCAN

import qualified Network.Socket

example :: IO ()
example = do
  let interface = "vcan0"
  mIdx <- Network.Socket.ifNameToIndex interface
  case mIdx of
    Nothing -> error $ "Interface " <> interface <> " not found"
    Just idx ->
      withSocketCAN idx act

act :: Socket -> IO ()
act sock = do
  sendCANMessage
    sock
    $ Network.CAN.standardMessage
        0x123
        [0xDE, 0xAD]

  sendCANMessage
    sock
    $ CANMessage
        (extendedID 0x123456)
        [0xEE]

  sendCANMessage
    sock
    $ CANMessage
        (setRTR $ extendedID 0x123)
        [0xDE, 0xAD, 0x11]

  forever
    $ recvCANMessage sock
    >>= print
