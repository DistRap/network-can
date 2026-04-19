module Main where

import Control.Monad.Class.MonadSay (MonadSay(say))
import Data.Default.Class (Default(def))
import System.Hardware.Serialport (CommSpeed(..), SerialPortSettings(..))
import Network.CAN (CANEndpoint)
import Network.SLCAN (Transport(..))

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

  Network.SLCAN.withSLCAN
    (Transport_Handle h)
    def
    act

act
  :: MonadSay m
  => CANEndpoint m
  -> m ()
act can = do
  Network.CAN.send
    can
    $ Network.CAN.standardMessage
        -- vendorID SDO
        0x601
        [0x40, 0x18, 0x10, 0x01, 0x0, 0x0, 0x0, 0x0]

  Control.Monad.forever
    $ Network.CAN.recv
        can
      >>= say . Network.CAN.prettyCANMessage
