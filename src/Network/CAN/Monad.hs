{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
module Network.CAN.Monad
  ( CANError(..)
  , MonadCAN(..)
  ) where

import Control.Monad.Except (MonadError)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State (StateT)
import Network.CAN.Types (CANMessage(..))

data CANError
  = CANError_NoSuchInterface String
  deriving (Eq, Show)

class ( MonadIO m
      , MonadError CANError m
      ) => MonadCAN m where

  send :: CANMessage -> m ()
  default send
    :: ( MonadTrans t
       , MonadCAN m'
       , m ~ t m'
       )
    => CANMessage
    -> m ()
  send = lift . send

  recv :: m CANMessage
  default recv
    :: ( MonadTrans t
       , MonadCAN m'
       , m ~ t m'
       )
    => m CANMessage
  recv = lift recv

instance MonadCAN m => MonadCAN (StateT s m)
instance MonadCAN m => MonadCAN (ReaderT r m)
instance MonadCAN m => MonadCAN (ExceptT CANError m)
