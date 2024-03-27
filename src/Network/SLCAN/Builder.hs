{-# LANGUAGE RecordWildCards #-}

module Network.SLCAN.Builder
  ( buildSLCANMessage
  ) where

import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Network.CAN.Types (CANArbitrationField(..), CANMessage(..))
import qualified Data.ByteString.Lazy
import qualified Data.ByteString.Builder

slCANBuilder
  :: CANMessage
  -> Builder
slCANBuilder CANMessage{..} =
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
  <> Data.ByteString.Builder.word32Hex
      canArbitrationFieldID

buildSLCANMessage
  :: CANMessage
  -> ByteString
buildSLCANMessage =
   Data.ByteString.Lazy.toStrict
 . Data.ByteString.Builder.toLazyByteString
 . slCANBuilder

