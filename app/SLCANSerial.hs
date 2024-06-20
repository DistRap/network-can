module Main where

import Data.Default.Class (Default(def))
import System.IO (Handle)
import System.Hardware.Serialport (CommSpeed(..), SerialPortSettings(..))

import qualified Control.Monad
import qualified Network.CAN
import qualified Network.SLCAN
import qualified System.Hardware.Serialport

main :: IO ()
main = do
  let
    port = "/dev/can4discouart"

  h <-
    System.Hardware.Serialport.hOpenSerial
      port
      (System.Hardware.Serialport.defaultSerialSettings
        { commSpeed = CS115200 }
      )

  Network.SLCAN.withSLCANHandle
    h
    def
    act

act :: Handle -> IO ()
act handle = do
  Network.SLCAN.sendCANMessage
    handle
    $ Network.CAN.standardMessage
        -- vendorID SDO
        0x601
        [0x40, 0x18, 0x10, 0x01, 0x0, 0x0, 0x0, 0x0]

  Control.Monad.forever
    $ Network.SLCAN.recvSLCANMessage handle
    >>= print
