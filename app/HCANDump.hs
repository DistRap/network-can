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

