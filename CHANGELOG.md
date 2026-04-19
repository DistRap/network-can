# Version [next](https://github.com/DistRap/network-can/compare/0.1.0.0...master) (2026-MM-DD)

* Split `slcan` and `socketcan` into public sublibraries
* Migrate to `io-classes` and switch from `MonadCAN` typeclass
  to `CANEndpoint` handle (record of functions style):

  ```
  data CANEndpoint m = CANEndpoint
  { canEndpointSend :: CANMessage -> m ()
  , canEndpointRecv :: m CANMessage
  }
  ```
* Runners renamed
  * `runSocketCAN` is now `withSocketCAN`
  * `runSLCAN` is now `withSLCAN`
  to reflect the handle change

# Version [0.1.0.0](https://github.com/DistRap/network-can/compare/d50564...0.1.0.0) (2025-05-19)

* Initial release

---

`network-can` uses [PVP Versioning][1].

[1]: https://pvp.haskell.org
