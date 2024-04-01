{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Concurrent.STM (TQueue)
import Data.Default.Class (Default(def))
import Network.CAN (CANMessage)
import Network.SLCAN (SLCANMessage(..))

import qualified Control.Monad
import qualified Control.Concurrent
import qualified Control.Concurrent.STM
import qualified Network.SocketCAN
import qualified Network.SLCAN
import qualified System.IO

-- bridge vcan0 to virtual /tmp/ttyV0 serial port
-- created with
-- socat -d -d pty,link=/tmp/ttyV0,raw,echo=0 pty,link=/tmp/ttyV1,raw,echo=0
-- so we can run ivory-tower-posix against it
-- ./build/canopen-posix-test/tower_init <> /tmp/ttyV1  > /tmp/ttyV1
main :: IO ()
main = do
  aToB <- Control.Concurrent.STM.newTQueueIO
  bToA <- Control.Concurrent.STM.newTQueueIO

  Control.Monad.void
    $ Control.Concurrent.forkIO
      $ socketCANSide aToB bToA
  slCANSide bToA aToB

socketCANSide
  :: TQueue CANMessage
  -> TQueue CANMessage
  -> IO ()
socketCANSide writeQ readQ = do
  let
    interface = "vcan0"
    recv sock =
      Control.Monad.forever
      $ Network.SocketCAN.recvCANMessage sock
        >>= Control.Concurrent.STM.atomically
            . Control.Concurrent.STM.writeTQueue writeQ
    send sock =
      Control.Monad.forever
      $ Control.Concurrent.STM.atomically
          (Control.Concurrent.STM.readTQueue readQ)
        >>= Network.SocketCAN.sendCANMessage sock

  mIdx <- Network.SocketCAN.ifNameToIndex interface
  case mIdx of
    Nothing -> error $ "Interface " <> interface <> " not found"
    Just idx ->
      Network.SocketCAN.withSocketCAN
        idx
        $ \sock -> do
            Control.Monad.void
              $ Control.Concurrent.forkIO
                $ recv sock
            send sock

slCANSide
  :: TQueue CANMessage
  -> TQueue CANMessage
  -> IO ()
slCANSide writeQ readQ = do
  let
    fp = "/tmp/ttyV0"

    recv handle =
      Control.Monad.forever
      $ Network.SLCAN.recvSLCANMessage handle
        >>= \case
          Left e ->
            error $ "SLCAN parse error: " <> e
          Right (SLCANMessage_Data cm) ->
            Control.Concurrent.STM.atomically
            $ Control.Concurrent.STM.writeTQueue writeQ cm
          Right other ->
            putStrLn $ "SLCAN: " <> show other
    send handle =
      Control.Monad.forever
      $ Control.Concurrent.STM.atomically
          (Control.Concurrent.STM.readTQueue readQ)
        >>= Network.SLCAN.sendCANMessage handle

  h <- System.IO.openFile
        fp
        System.IO.ReadWriteMode

  Network.SLCAN.withSLCANHandle
    h
    def
    $ \sh -> do
        Control.Monad.void
          $ Control.Concurrent.forkIO
            $ recv sh
        send sh
