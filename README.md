# network-can

[![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/DistRap/network-can/ci.yaml?branch=main)](https://github.com/DistRap/network-can/actions/workflows/ci.yaml)
[![Hackage version](https://img.shields.io/hackage/v/network-can.svg?color=success)](https://hackage.haskell.org/package/network-can)

CAN bus networking using Linux SocketCAN or SLCAN backends.

## Usage

```haskell
import qualified Control.Monad
import qualified Control.Monad.IO.Class
import qualified Network.CAN
import qualified Network.SocketCAN

main :: IO ()
main = do
  Network.SocketCAN.runSocketCAN
    (Network.SocketCAN.mkCANInterface "vcan0")
    $ do
        Network.CAN.send
          $ Network.CAN.standardMessage
              0x123
              [0xDE, 0xAD]

        Control.Monad.forever
          $ Network.CAN.recv
            >>= Control.Monad.IO.Class.liftIO . print
```
