{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

module Network.SLCAN.Parser
  ( parseSLCANMessage
  ) where

import Data.Attoparsec.ByteString.Char8 (Parser)
import Data.Bits (Bits)
import Data.ByteString (ByteString)
import Data.Set (Set)
import Control.Applicative ((<|>))
import Network.CAN.Types (CANArbitrationField(..), CANMessage(..))
import Network.SLCAN.Types
  ( SLCANMessage(..)
  , SLCANControl(..)
  , SLCANBitrate
  , SLCANState(..)
  , SLCANCounters(..)
  , SLCANError(..)
  )

import qualified Data.Attoparsec.ByteString.Char8
import qualified Data.Set
import qualified Control.Monad

slCANParser :: Parser SLCANMessage
slCANParser = do
  Data.Attoparsec.ByteString.Char8.peekChar'
    >>= \case
      c | Data.Attoparsec.ByteString.Char8.inClass "OCSFL" c ->
        (SLCANMessage_Control <$> slCANControlParser)
      c | Data.Attoparsec.ByteString.Char8.inClass "tTrR" c ->
        (SLCANMessage_Data <$> slCANDataParser)
      's' ->
        Data.Attoparsec.ByteString.Char8.char 's'
        *> (SLCANMessage_State <$> slCANStateParser <*> slCANCountersParser)
      'e' ->
        Data.Attoparsec.ByteString.Char8.char 'e'
        *> (SLCANMessage_Error <$> slCANErrorParser)
      c | otherwise ->
        fail $ "Unknown SLCAN message type: " <> show c
  <* Data.Attoparsec.ByteString.Char8.char '\r'

slCANControlParser :: Parser SLCANControl
slCANControlParser = do
  Data.Attoparsec.ByteString.Char8.anyChar
  >>= \case
    'O' -> pure SLCANControl_Open
    'C' -> pure SLCANControl_Close
    'S' -> (SLCANControl_Bitrate <$> bitrate)
    'F' -> pure SLCANControl_ResetErrors
    'L' -> pure SLCANControl_ListenOnly
    c   -> fail $ "Unknown control message char: " <> show c
  where
    bitrate = do
      d <- Data.Attoparsec.ByteString.Char8.decimal
      if d > fromEnum (maxBound :: SLCANBitrate)
      then fail
             $ "Bitrate out of bounds, got "
             <> show d
             <> "but maximum is "
             <> show (fromEnum (maxBound :: SLCANBitrate))
             <> " ("
             <> show  (maxBound :: SLCANBitrate)
             <> ")"
      else pure $ toEnum d

slCANDataParser :: Parser CANMessage
slCANDataParser = do
  canMessageArbitrationField <- arbitrationId
  msgLen <- hexadecimalWithLength 1
  canMessageData <-
    Control.Monad.replicateM msgLen (hexadecimalWithLength 2)
  pure CANMessage{..}

-- | Parse arbitration ID
-- * t => 11 bit data frame
-- * r => 11 bit RTR frame
-- * T => 29 bit data frame
-- * R => 29 bit RTR frame
arbitrationId :: Parser CANArbitrationField
arbitrationId = do
      Data.Attoparsec.ByteString.Char8.char 't' *> stdID False
  <|> Data.Attoparsec.ByteString.Char8.char 'r' *> stdID True
  <|> Data.Attoparsec.ByteString.Char8.char 'T' *> extID False
  <|> Data.Attoparsec.ByteString.Char8.char 'R' *> extID True

stdID
  :: Bool
  -> Parser CANArbitrationField
stdID isRTR = do
  canArbitrationFieldID
    <- hexadecimalWithLength 3

  let
    canArbitrationFieldExtended = False
    canArbitrationFieldRTR = isRTR

  pure CANArbitrationField{..}

extID
  :: Bool
  -> Parser CANArbitrationField
extID isRTR = do
  canArbitrationFieldID
    <- hexadecimalWithLength 8

  let
    canArbitrationFieldExtended = True
    canArbitrationFieldRTR = isRTR

  pure CANArbitrationField{..}

hexadecimalWithLength
  :: ( Bits a
     , Integral a
     )
  => Int
  -> Parser a
hexadecimalWithLength len =
  Data.Attoparsec.ByteString.Char8.take len
  >>=
  either
    fail
    pure
    . Data.Attoparsec.ByteString.Char8.parseOnly
        Data.Attoparsec.ByteString.Char8.hexadecimal

slCANStateParser :: Parser SLCANState
slCANStateParser =
  Data.Attoparsec.ByteString.Char8.anyChar
  >>= \case
    'a' -> pure SLCANState_Active
    'w' -> pure SLCANState_Warning
    'p' -> pure SLCANState_Passive
    'b' -> pure SLCANState_BusOff
    c   -> fail $ "Unknown state char: " <> show c

slCANCountersParser :: Parser SLCANCounters
slCANCountersParser = do
    slCANCountersTxErrors <- decimal3
    slCANCountersRxErrors <- decimal3
    pure $ SLCANCounters{..}
  where
    decimal3 =
      Data.Attoparsec.ByteString.Char8.take 3
      >>=
      either
        fail
        pure
        . Data.Attoparsec.ByteString.Char8.parseOnly
            Data.Attoparsec.ByteString.Char8.decimal

slCANErrorParser :: Parser (Set SLCANError)
slCANErrorParser = do
  len <- hexadecimalWithLength 1
  Data.Set.fromList
    <$> Control.Monad.replicateM len errorChar
  where
    errorChar =
      Data.Attoparsec.ByteString.Char8.anyChar
      >>= \case
        'a' -> pure SLCANError_Ack
        'b' -> pure SLCANError_Bit0
        'B' -> pure SLCANError_Bit1
        'c' -> pure SLCANError_CRC
        'f' -> pure SLCANError_Form
        'o' -> pure SLCANError_RxOverrun
        'O' -> pure SLCANError_RxOverrun
        's' -> pure SLCANError_Stuff
        c   -> fail $ "Unknown error char: " <> show c

parseSLCANMessage
  :: ByteString
  -> Either String SLCANMessage
parseSLCANMessage =
  Data.Attoparsec.ByteString.Char8.parseOnly
    slCANParser
