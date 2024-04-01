module Main where

import qualified Control.Monad
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
        (\sock ->
          Control.Monad.forever
            $ Network.SocketCAN.recvCANMessage sock
              >>= print
        )

-- needs Network.CAN.Pretty or Builder or smthing
-- that does the same ID formatting as SLCAN.Builder:78
-- a la
-- $ candump -e vcan0
--  vcan0  001237E5   [2]  4C EE
--  vcan0       7E5   [2]  4C EE
