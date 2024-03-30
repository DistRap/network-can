module Network.SLCAN.Types
  ( SLCANMessage(..)
  , SLCANControl(..)
  , SLCANBitrate(..)
  , SLCANState(..)
  , SLCANCounters(..)
  , SLCANError(..)
  , SLCANConfig(..)
  ) where

import Data.Default.Class (Default(def))
import Data.Set (Set)
import Data.Word (Word16)
import Network.CAN.Types (CANMessage)

data SLCANMessage
  = SLCANMessage_Control SLCANControl
  | SLCANMessage_Data CANMessage
  | SLCANMessage_State SLCANState SLCANCounters
  | SLCANMessage_Error (Set SLCANError)
  deriving (Eq, Ord, Show)

data SLCANControl
  = SLCANControl_Open
  | SLCANControl_Close
  | SLCANControl_Bitrate SLCANBitrate
  | SLCANControl_ResetErrors
  | SLCANControl_ListenOnly
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
  deriving (Bounded, Eq, Enum, Ord, Show)

instance Default SLCANBitrate where
  def = SLCANBitrate_1M

data SLCANState
  = SLCANState_Active
  | SLCANState_Warning
  | SLCANState_Passive
  | SLCANState_BusOff
  deriving (Eq, Ord, Show)

data SLCANCounters = SLCANCounters
  { slCANCountersRxErrors :: Word16
  , slCANCountersTxErrors :: Word16
  } deriving (Eq, Ord, Show)

data SLCANError
  = SLCANError_Ack
  | SLCANError_Bit0
  | SLCANError_Bit1
  | SLCANError_CRC
  | SLCANError_Form
  | SLCANError_RxOverrun
  | SLCANError_TxOverrun
  | SLCANError_Stuff
  deriving (Eq, Ord, Show)

data SLCANConfig = SLCANConfig
  { slCANConfigBitrate :: SLCANBitrate
  , slCANConfigResetErrors :: Bool
  , slCANConfigListenOnly :: Bool
  } deriving (Eq, Ord, Show)

instance Default SLCANConfig where
  def =
    SLCANConfig
    { slCANConfigBitrate = def
    , slCANConfigResetErrors = False
    , slCANConfigListenOnly = False
    }
