module Network.CAN
  ( CANEndpoint(..)
  , send
  , recv
  , module Network.CAN.Types
  ) where

import Network.CAN.Types

data CANEndpoint m = CANEndpoint
  { canEndpointSend :: CANMessage -> m ()
  , canEndpointRecv :: m CANMessage
  }

send
  :: CANEndpoint m
  -> CANMessage
  -> m ()
send = canEndpointSend

recv
  :: CANEndpoint m
  -> m CANMessage
recv = canEndpointRecv
