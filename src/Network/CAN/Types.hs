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
  , prettyCANMessage
  ) where

import Data.Word (Word8, Word16, Word32)
import Test.QuickCheck (Arbitrary(..))

import qualified Test.QuickCheck
import qualified Text.Printf

-- * Arbitration

data CANArbitrationField = CANArbitrationField
  { canArbitrationFieldID       :: Word32 -- ^ CAN ID
  , canArbitrationFieldExtended :: Bool   -- ^ Extended CAN ID
  , canArbitrationFieldRTR      :: Bool   -- ^ Remote transmission request
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

-- | Pretty print @CANMessage@ similar to candump output
--
-- > prettyCANMessage (standardMessage 123 [0x13, 0x37])
-- "     07B   [2]  13 37"
-- > prettyCANMessage (CANMessage (extendedID 123) [0x13, 0x37])
-- "0000007B   [2]  13 37"
prettyCANMessage
  :: CANMessage
  -> String
prettyCANMessage msg =
  unwords
    $ [ prettyArb
          $ canMessageArbitrationField msg
      , "  [" <> show (length $ canMessageData msg) <> "] "
      ]
      ++ prettyData
           (canArbitrationFieldRTR $ canMessageArbitrationField msg)
           (canMessageData msg)
  where
    prettyArb arb | canArbitrationFieldExtended arb =
      hexFixed
        8
        $ canArbitrationFieldID arb
    prettyArb arb | otherwise =
         replicate 5 ' '
      <> hexFixed
          3
          (canArbitrationFieldID arb)

    prettyData :: Bool -> [Word8] -> [String]
    prettyData True _ = pure "remote request"
    prettyData _    x = map (hexFixed 2) x

    hexFixed width =
      Text.Printf.printf
        $ "%0" <> show (width :: Int) <> "X"
