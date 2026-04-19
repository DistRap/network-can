{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad.Class.MonadAsync (race_)
import Data.Default.Class (Default(def))
import Network.SLCAN (Transport(..))
import System.Hardware.Serialport (CommSpeed(..), SerialPortSettings(..))

import qualified Control.Monad
import qualified Network.CAN
import qualified Network.SocketCAN
import qualified Network.SLCAN
import qualified System.Hardware.Serialport

-- | Bridge vcan0 to slcan over /dev/can4discouart serial port
main :: IO ()
main = do
  h <- System.Hardware.Serialport.hOpenSerial
    "/dev/can4discouart"
    (System.Hardware.Serialport.defaultSerialSettings
      { commSpeed = CS115200 }
    )
  Network.SLCAN.withSLCAN (Transport_Handle h) def $ \slcan -> do
    Network.SocketCAN.withSocketCAN (Network.SocketCAN.mkCANInterface "vcan0") $ \socketcan -> do
        race_
          (Control.Monad.forever
           $ Network.CAN.recv slcan >>= Network.CAN.send socketcan
          )
          (Control.Monad.forever
           $ Network.CAN.recv socketcan >>= Network.CAN.send slcan
          )
