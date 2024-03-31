module Network.CAN.Types
  (
  -- * Arbitration
    CANArbitrationField(..)
  , standardID
  , extendedID
  , setRTR
  -- * Message
  , CANMessage(..)
  , standardMessage
  ) where

import Data.Word (Word8, Word16, Word32)
import Test.QuickCheck (Arbitrary(..))

import qualified Test.QuickCheck

-- * Arbitration

data CANArbitrationField = CANArbitrationField
  { canArbitrationFieldID :: Word32 -- ^ CAN ID
  , canArbitrationFieldExtended :: Bool -- ^ Extended CAN ID
  , canArbitrationFieldRTR :: Bool -- ^ Remote transmission request
  } deriving (Eq, Ord, Show)

instance Arbitrary CANArbitrationField where
  arbitrary = do
    rtr <- arbitrary
    ext <- arbitrary
    cid <-
      if ext
      then Test.QuickCheck.choose (0, 0x3FFFFFFF)
      else Test.QuickCheck.choose (0, 0xFFF)
    pure
      CANArbitrationField
      { canArbitrationFieldID = cid
      , canArbitrationFieldExtended = ext
      , canArbitrationFieldRTR = rtr
      }

-- | Construct standard CAN ID (11 bits)
standardID
  :: Word16
  -> CANArbitrationField
standardID cid = CANArbitrationField
  { canArbitrationFieldID = fromIntegral cid
  , canArbitrationFieldExtended = False
  , canArbitrationFieldRTR = False
  }

-- | Construct extended CAN ID (29 bits)
extendedID
  :: Word32
  -> CANArbitrationField
extendedID cid = CANArbitrationField
  { canArbitrationFieldID = cid
  , canArbitrationFieldExtended = True
  , canArbitrationFieldRTR = False
  }

-- | Set remote transmission request bit
setRTR
  :: CANArbitrationField
  -> CANArbitrationField
setRTR cf = cf { canArbitrationFieldRTR = True }

data CANMessage = CANMessage
  { canMessageArbitrationField :: CANArbitrationField
  , canMessageData :: [Word8]
  } deriving (Eq, Ord, Show)

instance Arbitrary CANMessage where
  arbitrary = do
    arb <- arbitrary
    len <- Test.QuickCheck.choose (0, 8)
    dat <- Test.QuickCheck.vectorOf len arbitrary
    pure
      CANMessage
      { canMessageArbitrationField = arb
      , canMessageData = dat
      }

-- | Create standard CAN message
standardMessage
  :: Word16
  -> [Word8]
  -> CANMessage
standardMessage cid cdata = CANMessage
  { canMessageArbitrationField = standardID cid
  , canMessageData = cdata
  }
