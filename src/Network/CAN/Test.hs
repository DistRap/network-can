module Network.CAN.Test where

import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO(liftIO))

import Network.CAN
import Network.SLCAN
import Network.SocketCAN

import Data.Default.Class

tst :: IO (Either CANError ())
tst = do
  runSocketCAN "vcan0" $ do
    send
      $ Network.CAN.standardMessage
          0x123
          [0xDE, 0xAD]

    send
      $ CANMessage
          (extendedID 0x123456)
          [0xEE]

    send
      $ CANMessage
          (setRTR $ extendedID 0x123)
          [0xDE, 0xAD, 0x11]

    forever
      $ recv
      >>= liftIO . print

tstCO :: IO (Either CANError ())
tstCO = do
  -- socat -d -d pty,link=/tmp/ttyV0,raw,echo=0 pty,link=/tmp/ttyV1,raw,echo=0
  -- ./build/canopen-posix-test/tower_init <> /tmp/ttyV1  > /tmp/ttyV1
  runSLCANFilePath "/tmp/ttyV0" def $ do
    send
      $ Network.CAN.standardMessage
          0x7E5
          [0x4C]

    recv
      >>= liftIO . print
