# network-can

[![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/DistRap/network-can/ci.yaml?branch=main)](https://github.com/DistRap/network-can/actions/workflows/ci.yaml)
[![Hackage version](https://img.shields.io/hackage/v/network-can.svg?color=success)](https://hackage.haskell.org/package/network-can)

CAN bus networking using Linux SocketCAN or SLCAN backends.

## Usage

```haskell
import qualified Control.Monad
import qualified Network.CAN
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
        (\sock -> do

          Network.SocketCAN.sendCANMessage
            sock
            $ Network.CAN.standardMessage
                0x123
                [0xDE, 0xAD]

          Control.Monad.forever
            $ Network.SocketCAN.recvCANMessage sock
              >>= print
        )
```
