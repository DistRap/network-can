# network-can

[![GitHub Workflow Status](https://img.shields.io/github/actions/workflow/status/DistRap/network-can/ci.yaml?branch=main)](https://github.com/DistRap/network-can/actions/workflows/ci.yaml)
[![Hackage version](https://img.shields.io/hackage/v/network-can.svg?color=success)](https://hackage.haskell.org/package/network-can)
[![Dependencies](https://img.shields.io/hackage-deps/v/network-can?label=Dependencies)](https://packdeps.haskellers.com/feed?needle=network-can)

CAN bus networking using Linux SocketCAN or SLCAN backends.

## Usage

```haskell
import qualified Network.CAN
import qualified Network.SocketCAN

main :: IO ()
main = do
  canSock <-
    Network.SocketCAN.open
      "vcan0"

  msg <-
    Network.CAN.standardMessage
      0x123
      [0xDE, 0xAD]

  Network.SocketCAN.send
    canSock
    msg

  print <$> Network.SocketCAN.recv canSock
  Network.SocketCAN.close canSock
```
