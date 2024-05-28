{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeOperators #-}
module Network.CAN.Class
  ( MonadCAN(..)
  ) where

import Control.Monad.Trans (MonadTrans, lift)
import Control.Monad.Trans.Except (ExceptT)
import Control.Monad.Trans.Reader (ReaderT)
import Control.Monad.Trans.State (StateT)
import Network.CAN.Types (CANMessage(..))

class Monad m => MonadCAN m where

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

instance MonadCAN m => MonadCAN (ExceptT e m)
instance MonadCAN m => MonadCAN (ReaderT r m)
instance MonadCAN m => MonadCAN (StateT s m)
