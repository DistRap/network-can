module Samples
  ( samples
  ) where

import Network.CAN

samples :: [CANMessage]
samples =
  [
  -- standard
    standardMessage
      0x0
      mempty

  , standardMessage
      0xFFF
      mempty

  , standardMessage
      0x123
      [0xDE, 0xAD]

  , CANMessage
      (setRTR $ standardID 0x123)
      mempty

  -- extended
  , CANMessage
      (extendedID 0x0)
      mempty

  , CANMessage
      (extendedID 0x123456)
      [0xEE]

  , CANMessage
      (setRTR $ extendedID 0x123456)
      mempty
  ]
