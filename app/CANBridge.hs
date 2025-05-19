{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad.Trans (MonadTrans(lift))
import Data.Default.Class (Default(def))
import Network.SLCAN (Transport(..))
import System.Hardware.Serialport (CommSpeed(..), SerialPortSettings(..))

import qualified Control.Monad
import qualified Network.CAN
import qualified Network.SocketCAN
import qualified Network.SLCAN
import qualified System.Hardware.Serialport
import qualified UnliftIO.Async

-- | Bridge vcan0 to slcan over /dev/can4discouart serial port
main :: IO ()
main = do
  h <- System.Hardware.Serialport.hOpenSerial
    "/dev/can4discouart"
    (System.Hardware.Serialport.defaultSerialSettings
      { commSpeed = CS115200 }
    )
  Network.SLCAN.runSLCAN (Transport_Handle h) def $ do
    Network.SocketCAN.runSocketCAN (Network.SocketCAN.mkCANInterface "vcan0") $ do
        UnliftIO.Async.race_
          (Control.Monad.forever
           $ Network.CAN.recv >>= lift . Network.CAN.send
          )
          (Control.Monad.forever
           $ lift Network.CAN.recv >>= Network.CAN.send
          )
