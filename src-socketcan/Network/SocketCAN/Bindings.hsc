{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Network.SocketCAN.Bindings
  (
  -- * Network package compatibility
    SockAddrCAN(..)
  , pattern CAN_RAW
  -- * SocketCAN bindings
  , SocketCANArbitrationField(..)
  , SocketCANFrame(..)
  ) where

import Data.Word (Word8, Word16, Word32)
import Foreign.Storable (Storable(..))
import Foreign.Marshal.Array (peekArray, pokeArray)
import Foreign.Ptr (plusPtr)
import Network.Socket.Address (SocketAddress(..))
import Network.Socket (ProtocolNumber)

#include <linux/can.h>
#include <sys/socket.h>

-- | CAN Socket type
newtype SockAddrCAN = SockAddrCAN Word32
  deriving (Eq, Ord)

-- Word16
type CSaFamily = (#type sa_family_t)

instance SocketAddress SockAddrCAN where
  sizeOfSocketAddress (SockAddrCAN _) =
    #const sizeof(struct sockaddr_can)
  peekSocketAddress sap = do
    ifidx <- (#peek struct sockaddr_can, can_ifindex) sap
    return (SockAddrCAN ifidx)
  pokeSocketAddress p (SockAddrCAN ifIndex) = do
    (#poke struct sockaddr_can, can_family) p ((#const AF_CAN) :: CSaFamily)
    (#poke struct sockaddr_can, can_ifindex) p ifIndex

-- | CAN RAW protocol family of PF_CAN
pattern CAN_RAW :: ProtocolNumber
pattern CAN_RAW = #const CAN_RAW

-- | SocketCAN Arbitration field (CAN ID including RTR, EFF, ERR bits)
newtype SocketCANArbitrationField =
  SocketCANArbitrationField { unSocketCANArbitrationField :: Word32 }
  deriving (Eq, Ord, Show, Storable)

data SocketCANFrame = SocketCANFrame
  { socketCANFrameArbitrationField :: SocketCANArbitrationField
  , socketCANFrameLength :: Word8
  , socketCANFrameData   :: [Word8]
  } deriving Show

instance Storable SocketCANFrame where
  sizeOf ~_ = #const sizeof(struct can_frame)
  alignment ~_ = #alignment struct can_frame
  peek ptr = do
    socketCANFrameArbitrationField
      <- #{peek struct can_frame, can_id} ptr
    socketCANFrameLength
      <- #{peek struct can_frame, len} ptr
    socketCANFrameData <-
      peekArray
        (fromIntegral socketCANFrameLength)
        (#{ptr struct can_frame, data} ptr)
    pure
      $ SocketCANFrame{..}
  poke ptr SocketCANFrame{..} = do
    #{poke struct can_frame, can_id}
      ptr
      socketCANFrameArbitrationField
    #{poke struct can_frame, len}
      ptr
      socketCANFrameLength
    pokeArray
      (#{ptr struct can_frame, data} ptr)
      socketCANFrameData
