{-# LANGUAGE NumericUnderscores #-}
module Network.SLCAN.Types
  ( SLCANMessage(..)
  , SLCANControl(..)
  , SLCANBitrate(..)
  , numericBitrate
  , SLCANState(..)
  , SLCANCounters(..)
  , SLCANError(..)
  , SLCANConfig(..)
  ) where

import Data.Default.Class (Default(def))
import Data.Set (Set)
import Data.Word (Word16)
import Network.CAN.Types (CANMessage)
import Test.QuickCheck (Arbitrary(..))

import qualified Test.QuickCheck

data SLCANMessage
  = SLCANMessage_Control SLCANControl
  | SLCANMessage_Data CANMessage
  | SLCANMessage_State SLCANState SLCANCounters
  | SLCANMessage_Error (Set SLCANError)
  deriving (Eq, Ord, Show)

instance Arbitrary SLCANMessage where
  arbitrary = Test.QuickCheck.oneof
    [ SLCANMessage_Control <$> arbitrary
    , SLCANMessage_Data <$> arbitrary
    , SLCANMessage_State <$> arbitrary <*> arbitrary
    , SLCANMessage_Error <$> arbitrary
    ]

data SLCANControl
  = SLCANControl_Open
  | SLCANControl_Close
  | SLCANControl_Bitrate SLCANBitrate
  | SLCANControl_ResetErrors
  | SLCANControl_ListenOnly
  deriving (Eq, Ord, Show)

instance Arbitrary SLCANControl where
  arbitrary = Test.QuickCheck.oneof
    [ pure SLCANControl_Open
    , pure SLCANControl_Close
    , SLCANControl_Bitrate <$> arbitrary
    , pure SLCANControl_ResetErrors
    , pure SLCANControl_ListenOnly
    ]

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

instance Arbitrary SLCANBitrate where
  arbitrary = Test.QuickCheck.arbitraryBoundedEnum

instance Default SLCANBitrate where
  def = SLCANBitrate_1M

numericBitrate :: SLCANBitrate -> Int
numericBitrate SLCANBitrate_10K  =    10_000
numericBitrate SLCANBitrate_20K  =    20_000
numericBitrate SLCANBitrate_50K  =    50_000
numericBitrate SLCANBitrate_100K =   100_000
numericBitrate SLCANBitrate_125K =   125_000
numericBitrate SLCANBitrate_250K =   250_000
numericBitrate SLCANBitrate_500K =   500_000
numericBitrate SLCANBitrate_800K =   800_000
numericBitrate SLCANBitrate_1M   = 1_000_000

data SLCANState
  = SLCANState_Active
  | SLCANState_Warning
  | SLCANState_Passive
  | SLCANState_BusOff
  deriving (Bounded, Eq, Enum, Ord, Show)

instance Arbitrary SLCANState where
  arbitrary = Test.QuickCheck.arbitraryBoundedEnum

data SLCANCounters = SLCANCounters
  { slCANCountersRxErrors :: Word16
  , slCANCountersTxErrors :: Word16
  } deriving (Eq, Ord, Show)

instance Arbitrary SLCANCounters where
  arbitrary =
    SLCANCounters
      <$> Test.QuickCheck.choose (0, 999)
      <*> Test.QuickCheck.choose (0, 999)

data SLCANError
  = SLCANError_Ack
  | SLCANError_Bit0
  | SLCANError_Bit1
  | SLCANError_CRC
  | SLCANError_Form
  | SLCANError_RxOverrun
  | SLCANError_TxOverrun
  | SLCANError_Stuff
  deriving (Bounded, Eq, Enum, Ord, Show)

instance Arbitrary SLCANError where
  arbitrary = Test.QuickCheck.arbitraryBoundedEnum

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
