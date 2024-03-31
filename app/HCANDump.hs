module Main where

import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Network.CAN
import Network.SLCAN
import Network.SocketCAN

main :: IO () -- Either CANError ())
main = do
  void $ runSocketCAN "vcan0" $ do
    forever $ recv >>= liftIO . print

-- needs Network.CAN.Pretty or Builder or smthing
-- that does the same ID formatting as SLCAN.Builder:78
-- a la
-- $ candump -e vcan0
--  vcan0  001237E5   [2]  4C EE
--  vcan0       7E5   [2]  4C EE
