module Main where

import Control.Concurrent.Classy.Async
import Control.Monad (forever, void)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad.Trans (lift)
import Data.Default.Class
import Network.CAN
import Network.SLCAN
import Network.SocketCAN

main :: IO ()
main = do
  -- newTQueue aToB
  -- newTQueue bToA
  void $ runSLCANFilePath "/tmp/ttyV1" def $ do
     void $ runSocketCAN "vcan0" $ do
        (forever $ lift recv >>= send)
        (forever $ recv >>= lift . send)
