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

  System.Hardware.Serialport.hWithSerial
    port
    (System.Hardware.Serialport.defaultSerialSettings
      { commSpeed = CS115200 }
    )
    $ \h ->
        Network.SLCAN.withSLCANHandle
          h
          def
          act

act :: Handle -> IO ()
act handle = do
  Network.SLCAN.sendCANMessage
    handle
    $ Network.CAN.standardMessage
        0x7E5
        [0x4C]

  Control.Monad.forever
    $ Network.SLCAN.recvSLCANMessage handle
    >>= print
