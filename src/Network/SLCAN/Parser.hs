{-# LANGUAGE RecordWildCards #-}

module Network.SLCAN.Parser
  ( parseSLCANMessage
  ) where

import Data.Attoparsec.ByteString.Char8 (Parser)
import Data.Bits (Bits)
import Data.ByteString (ByteString)
import Control.Applicative ((<|>))
import Network.CAN.Types (CANArbitrationField(..), CANMessage(..))

import qualified Data.Attoparsec.ByteString.Char8
import qualified Control.Monad

slCANParser :: Parser CANMessage
slCANParser = do
  canMessageArbitrationField <- arbitrationId
  msgLen <- hexadecimalWithLength 1
  canMessageData <-
    Control.Monad.replicateM msgLen (hexadecimalWithLength 2)
  _ <- Data.Attoparsec.ByteString.Char8.char '\r'
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

parseSLCANMessage
  :: ByteString
  -> Either String CANMessage
parseSLCANMessage =
  Data.Attoparsec.ByteString.Char8.parseOnly
    slCANParser
