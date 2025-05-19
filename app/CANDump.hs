module Main where

import qualified Control.Monad
import qualified Control.Monad.IO.Class
import qualified Network.CAN
import qualified Network.SocketCAN

main :: IO ()
main = do
  let interface = "vcan0"
  Network.SocketCAN.runSocketCAN interface
    (Control.Monad.forever
       $ Network.CAN.recv
         >>= Control.Monad.IO.Class.liftIO . print
    )

-- needs Network.CAN.Pretty or Builder or smthing
-- that does the same ID formatting as SLCAN.Builder:78
-- a la
-- $ candump -e vcan0
--  vcan0  001237E5   [2]  4C EE
--  vcan0       7E5   [2]  4C EE
