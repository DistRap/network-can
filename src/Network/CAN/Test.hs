module Network.CAN.Test where

import Control.Monad (forever)
import Control.Monad.IO.Class (MonadIO(liftIO))

import Network.CAN
import Network.SocketCAN

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
