module Network.SLCAN.Types where
  ( SLCANMessage(..)
  ) where

import Data.Default.Class (Default(def))

data SLCANMessage
  = SLCANMessage_Control SLCANControl
  | SLCANMessage_Data CANMessage
  deriving (Eq, Ord, Show)

data SLCANControl
  = SLCANControl_Open
  | SLCANControl_Close
  | SLCANControl_Speed SLCANBitrate
  deriving (Eq, Ord, Show)

data SLCANBitrate
  = SLCANBitrate_10K
  | SLCANBitrate_20K
  | SLCANBitrate_50K
  | SLCANBitrate_100K
  | SLCANBitrate_125K
  | SLCANBitrate_250K
  | SLCANBitrate_500K
  | SLCANBitrate_800K
  | SLCANBitrate_1M
  deriving (Eq, Enum, Ord, Show)

instance Default SLCANBitrate where
  def = SLCANBitrate_1M
