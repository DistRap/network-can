module Main where

import qualified Control.Monad
import qualified Network.CAN
import qualified Network.SocketCAN

main :: IO ()
main = do
  Network.SocketCAN.withSocketCAN
    (Network.SocketCAN.mkCANInterface "vcan0")
    $ \can ->
      (Control.Monad.forever
         $ Network.CAN.recv
             can
           >>= putStrLn . Network.CAN.prettyCANMessage
      )
