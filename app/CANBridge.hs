module Main where

import Control.Monad (forever, void)
import Data.Default.Class
import Network.CAN
import Network.SLCAN
import Network.SocketCAN
import qualified Network.SocketCAN

main :: IO ()
main = do
  let interface = "vcan0"
  mIdx <- Network.SocketCAN.ifNameToIndex interface
  case mIdx of
    Nothing -> error $ "Interface " <> interface <> " not found"
    Just idx ->
      Network.SocketCAN.withSocketCAN
        idx
        undefined
        -- async...

{--
  void $ runSLCANFilePath "/tmp/ttyV1" def $ do
     void $ runSocketCAN "vcan0" $ do
        (forever $ lift recv >>= send)
        (forever $ recv >>= lift . send)
--}
