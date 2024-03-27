{-# LANGUAGE RecordWildCards #-}

module Network.SLCAN.Parse
  ( parseSLCANMessage
  ) where

import Data.Attoparsec.ByteString.Char8 (Parser)
import Data.Bits (Bits)
import Data.ByteString (ByteString)
import Control.Applicative ((<|>))
import Network.CAN.Types (CANArbitrationField(..), CANMessage(..))

import qualified Data.Attoparsec.ByteString.Char8
import qualified Control.Monad

-- t10021133
-- std
slCANParser :: Parser CANMessage
slCANParser = do
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
  <|> Data.Attoparsec.ByteString.Char8.char 'r' *> extID True

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
 {--
 pure
   $ sum
   $ map
      (\(ascii, shift) ->
        Data.Bits.shiftL
          (fromIntegral $ toBin ascii)
          (shift * 4)
      )
   $ zip
       (Data.ByteString.Char8.unpack bs)
       [len - 1, len -2 ..]

toBin :: Char -> Word8
toBin x | x `elem` ['0'..'9'] =
  fromIntegral
    $ Data.Char.ord x - Data.Char.ord '0'
toBin x | x `elem` ['A'..'F'] =
  fromIntegral
    $ (Data.Char.ord x - Data.Char.ord 'A') + 10
toBin x | otherwise =
  fromIntegral
    $ (Data.Char.ord x - Data.Char.ord 'a') + 10
  --}

parseSLCANMessage
  :: ByteString
  -> Either String CANMessage
parseSLCANMessage =
  Data.Attoparsec.ByteString.Char8.parseOnly
    slCANParser
