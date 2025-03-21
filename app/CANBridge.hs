{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad.Trans (MonadTrans(lift))
import Data.Default.Class (Default(def))
import System.Hardware.Serialport (CommSpeed(..), SerialPortSettings(..))

import qualified Control.Monad
import qualified Network.CAN
import qualified Network.SocketCAN
import qualified Network.SLCAN
import qualified System.Hardware.Serialport
import qualified System.IO

import qualified UnliftIO.Async

-- bridge vcan0 to virtual /tmp/ttyV0 serial port
-- created with
-- socat -d -d pty,link=/tmp/ttyV0,raw,echo=0 pty,link=/tmp/ttyV1,raw,echo=0
-- so we can run ivory-tower-posix against it
-- ./build/canopen-posix-test/tower_init <> /tmp/ttyV1  > /tmp/ttyV1
main :: IO ()
main = do
  h <- System.Hardware.Serialport.hOpenSerial
    "/dev/can4discouart"
    (System.Hardware.Serialport.defaultSerialSettings
      { commSpeed = CS115200 }
    )
  Network.SLCAN.runSLCAN h def $ do
    Network.SocketCAN.runSocketCAN "vcan0" $ do
        UnliftIO.Async.race_
          (Control.Monad.forever
           $ Network.CAN.recv >>= lift . Network.CAN.send
          )
          (Control.Monad.forever
           $ lift Network.CAN.recv >>= Network.CAN.send
          )
