{-# LANGUAGE RecordWildCards #-}

-- | Translation between CANMessage and SocketCANFrame

module Network.SocketCAN.Translate
  ( toSocketCANFrame
  , fromSocketCANFrame
  ) where

import Data.Bits ((.&.), (.|.), shiftL)
import Data.Word (Word32)
import Network.CAN.Types (CANArbitrationField(..), CANMessage(..))
import Network.SocketCAN.Bindings (SocketCANArbitrationField(..), SocketCANFrame(..))
import qualified Data.Bool

toSocketCANFrame
  :: CANMessage
  -> SocketCANFrame
toSocketCANFrame CANMessage{..} =
  SocketCANFrame
  { socketCANFrameArbitrationField =
      toSocketCANArbitrationField
        canMessageArbitrationField
  , socketCANFrameLength =
      fromIntegral $ length canMessageData
  , socketCANFrameData = canMessageData
  }

fromSocketCANFrame
  :: SocketCANFrame
  -> CANMessage
fromSocketCANFrame SocketCANFrame{..} =
  CANMessage
  { canMessageArbitrationField =
      fromSocketCANArbitrationField
        socketCANFrameArbitrationField
  , canMessageData = socketCANFrameData
  }

toSocketCANArbitrationField
  :: CANArbitrationField
  -> SocketCANArbitrationField
toSocketCANArbitrationField CANArbitrationField{..} =
  SocketCANArbitrationField
  $ Data.Bool.bool
      id
      (.|. effBit)
      canArbitrationFieldExtended
  $ Data.Bool.bool
      id
      (.|. rtrBit)
      canArbitrationFieldRTR
  $ canArbitrationFieldID

fromSocketCANArbitrationField
  :: SocketCANArbitrationField
  -> CANArbitrationField
fromSocketCANArbitrationField (SocketCANArbitrationField scid) =
  let
    isEff = scid .&. effBit /= 0
  in
    CANArbitrationField
    { canArbitrationFieldID =
        Data.Bool.bool
          (.&. (1 `shiftL` 12 - 1))
          (.&. (1 `shiftL` 30 - 1))
          isEff
        $ scid
    , canArbitrationFieldExtended = isEff
    , canArbitrationFieldRTR = scid .&. rtrBit /= 0
    }

effBit :: Word32
effBit = 1 `shiftL` 31

rtrBit :: Word32
rtrBit = 1 `shiftL` 30
