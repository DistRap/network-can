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
  Network.SocketCAN.withSocketCAN
    (Network.SocketCAN.mkCANInterface "vcan0")
    $ \can -> do
        Network.CAN.send
          can
          $ Network.CAN.standardMessage
              0x123
              [0xDE, 0xAD]

        Control.Monad.forever
          $ Network.CAN.recv
              can
            >>= putStrLn . Network.CAN.prettyCANMessage
```
