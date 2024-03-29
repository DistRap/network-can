{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Network.SLCAN.Builder
  ( buildSLCANMessage
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.Set (Set)
import Network.CAN.Types (CANArbitrationField(..), CANMessage(..))
import Network.SLCAN.Types
  ( SLCANMessage(..)
  , SLCANControl(..)
  , SLCANState(..)
  , SLCANCounters(..)
  , SLCANError(..)
  )
import qualified Data.Bits
import qualified Data.Set
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Builder

slCANBuilder
  :: SLCANMessage
  -> Builder
slCANBuilder slcanMsg =
  case slcanMsg of
    SLCANMessage_Control ctrlMsg -> slCANControlBuilder ctrlMsg
    SLCANMessage_Data canMsg -> slCANDataBuilder canMsg
    SLCANMessage_State state counters  -> slCANStateBuilder state counters
    SLCANMessage_Error errs -> slCANErrorBuilder errs
  <> Data.ByteString.Builder.char7 '\r'

slCANControlBuilder
  :: SLCANControl
  -> Builder
slCANControlBuilder SLCANControl_Open =
  Data.ByteString.Builder.char7 'O'
slCANControlBuilder SLCANControl_Close =
  Data.ByteString.Builder.char7 'C'
slCANControlBuilder (SLCANControl_Bitrate speed) =
     Data.ByteString.Builder.char7 'S'
  <> Data.ByteString.Builder.intDec
      (fromEnum speed)
slCANControlBuilder SLCANControl_ResetErrors =
  Data.ByteString.Builder.char7 'F'
slCANControlBuilder SLCANControl_ListenOnly =
  Data.ByteString.Builder.char7 'L'

slCANDataBuilder
  :: CANMessage
  -> Builder
slCANDataBuilder CANMessage{..} =
     arbitrationId canMessageArbitrationField
  <> Data.ByteString.Builder.word8Hex
      (fromIntegral $ length canMessageData)
  <> mconcat
      (map
         Data.ByteString.Builder.word8Hex
         canMessageData
      )

arbitrationId
  :: CANArbitrationField
  -> Builder
arbitrationId CANArbitrationField{..} =
     Data.ByteString.Builder.char7
       (case ( canArbitrationFieldExtended
             , canArbitrationFieldRTR
             )
        of
         (False, False) -> 't'
         (False, True)  -> 'r'
         (True, False)  -> 'T'
         (True, True)   -> 'R'
       )
  <> (if canArbitrationFieldExtended
      then Data.ByteString.Builder.word32HexFixed
      else
        (\word11 ->
           Data.ByteString.Builder.word8Hex
            (fromIntegral (word11 `Data.Bits.shiftR` 8))
        <> Data.ByteString.Builder.word8HexFixed
            (fromIntegral word11)
        )
     )
     canArbitrationFieldID

slCANStateBuilder
  :: SLCANState
  -> SLCANCounters
  -> Builder
slCANStateBuilder state SLCANCounters{..} =
     Data.ByteString.Builder.char7 's'
  <> Data.ByteString.Builder.char7
      (case state of
         SLCANState_Active -> 'a'
         SLCANState_Warning -> 'w'
         SLCANState_Passive -> 'p'
         SLCANState_BusOff -> 'b'
      )
  <> word16Dec3 slCANCountersTxErrors
  <> word16Dec3 slCANCountersRxErrors
  where
    -- encode as 3 bytes (maximum of 999 and zero padded)
    word16Dec3 x =
           (case x of
              _ | x <= 10 -> Data.ByteString.Builder.string7 "00"
              _ | x <= 100 -> Data.ByteString.Builder.char7 '0'
              _ | otherwise -> mempty
           )
        <> Data.ByteString.Builder.word16Dec
            (min 999 x)

slCANErrorBuilder
  :: Set SLCANError
  -> Builder
slCANErrorBuilder errs =
     Data.ByteString.Builder.char7 'e'
  <> Data.ByteString.Builder.word8Hex
      (fromIntegral $ Data.Set.size errs)
  <> mconcat
      (map
         ( Data.ByteString.Builder.char7
         . \case
             SLCANError_Ack -> 'a'
             SLCANError_Bit0 -> 'b'
             SLCANError_Bit1 -> 'B'
             SLCANError_CRC -> 'c'
             SLCANError_Form -> 'f'
             SLCANError_RxOverrun -> 'o'
             SLCANError_TxOverrun -> 'O'
             SLCANError_Stuff -> 's'
         )
         $ Data.Set.toList
             errs
      )

buildSLCANMessage
  :: SLCANMessage
  -> ByteString
buildSLCANMessage =
   Data.ByteString.Lazy.toStrict
 . Data.ByteString.Builder.toLazyByteString
 . slCANBuilder
