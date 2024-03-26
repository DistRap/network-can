module Network.CAN
  ( CANArbitrartionField(..)
  , standardCANID
  , extendedCANID
  , setRTR
  , CANMessage(..)
  , standardCANMessage
  ) where

import Data.Word (Word8, Word16, Word32)

data CANArbitrartionField = CANArbitrationField
  { canArbitrationFieldID :: Word32 -- ^ CAN ID
  , canArbitrationFieldExtended :: Bool -- ^ Extended CAN ID
  , canArbitrationFieldRTR :: Bool -- ^ Remote transmission request
  } deriving (Eq, Ord, Show)

-- | Construct standard CAN ID (11 bits)
standardCANID
  :: Word16
  -> CANArbitrartionField
standardCANID cid = CANArbitrationField
  { canArbitrationFieldID = fromIntegral cid
  , canArbitrationFieldExtended = False
  , canArbitrationFieldRTR = False
  }

-- | Construct extended CAN ID (29 bits)
extendedCANID
  :: Word32
  -> CANArbitrartionField
extendedCANID cid = CANArbitrationField
  { canArbitrationFieldID = cid
  , canArbitrationFieldExtended = True
  , canArbitrationFieldRTR = False
  }

-- | Set remote transmission request bit
setRTR
  :: CANArbitrartionField
  -> CANArbitrartionField
setRTR cf = cf { canArbitrationFieldRTR = True }

data CANMessage = CANMessage
  { canMessageArbitrationField :: CANArbitrartionField
  , canMessageData :: [Word8]
  } deriving (Eq, Ord, Show)

-- | Create standard CAN message
standardCANMessage
  :: Word16
  -> [Word8]
  -> CANMessage
standardCANMessage cid cdata = CANMessage
  { canMessageArbitrationField = standardCANID cid
  , canMessageData = cdata
  }
