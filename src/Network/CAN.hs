module Network.CAN
  ( CAN(..)
  , send
  , recv
  , module Network.CAN.Types
  ) where

import Network.CAN.Types

data CAN m = CAN
  { canSend :: CANMessage -> m ()
  , canRecv :: m CANMessage
  }

send
  :: CAN m
  -> CANMessage
  -> m ()
send = canSend

recv
  :: CAN m
  -> m CANMessage
recv = canRecv
