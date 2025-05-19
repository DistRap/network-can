module Main where

import Control.Monad.IO.Class
import Data.Default.Class (Default(def))
import Network.CAN (MonadCAN)
import Network.SLCAN (Transport(..))
import Network.Socket (AddrInfo(..), SocketType(Datagram))

import qualified Control.Monad
import qualified Network.CAN
import qualified Network.SLCAN
import qualified Network.Socket

main :: IO ()
main = do
  ourAddrinfos
    <- Network.Socket.getAddrInfo
         Nothing
         Nothing
         (Just "1337")

  targetAddrinfos
    <- Network.Socket.getAddrInfo
         Nothing
         Nothing
         (Just "1338")

  case (ourAddrinfos, targetAddrinfos) of
    (ourAddrinfo:_, targetAddrinfo:_) -> do
      sock <- Network.Socket.socket
        (addrFamily ourAddrinfo)
        Datagram
        Network.Socket.defaultProtocol

      Network.Socket.bind
        sock
        (addrAddress ourAddrinfo)

      Network.SLCAN.runSLCAN
        (Transport_UDP sock (addrAddress targetAddrinfo))
        def
        act

    (_, _) -> error "getAddrInfo fail"

act
  :: ( MonadCAN m
     , MonadIO m
     )
  => m ()
act = do
  Network.CAN.send
    $ Network.CAN.standardMessage
        0x7E5
        [0x4C]

  Control.Monad.forever
    $ Network.CAN.recv
      >>= Control.Monad.IO.Class.liftIO . print
