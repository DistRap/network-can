{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
module Network.SLCAN
  ( withSLCANHandle
  , sendSLCANMessage
  , sendSLCANControl
  , recvSLCANMessage
  , sendCANMessage
  , module Network.SLCAN.Types
  , SLCANT(..)
  , runSLCAN
  ) where

import Control.Exception (Exception)
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Reader (MonadReader, ask)
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.Trans.Reader (ReaderT(..))

import Network.CAN (CANMessage, MonadCAN(..))
import Network.SLCAN.Types
import System.IO (Handle)
import UnliftIO (MonadUnliftIO)

import qualified Control.Monad
import qualified Control.Exception
import qualified Data.ByteString
import qualified Data.ByteString.Char8
import qualified System.IO
import qualified Network.SLCAN.Builder
import qualified Network.SLCAN.Parser
import qualified UnliftIO

withSLCANHandle
  :: Handle
  -> SLCANConfig
  -> (Handle -> IO a)
  -> IO a
withSLCANHandle handle SLCANConfig{..} act = do
  let sendC = sendSLCANControl handle
  Control.Exception.finally
    (do
       sendC SLCANControl_Close
       sendC (SLCANControl_Bitrate slCANConfigBitrate)
       Control.Monad.when
         slCANConfigResetErrors
         (sendC SLCANControl_ResetErrors)
       sendC
         (if slCANConfigListenOnly
          then SLCANControl_ListenOnly
          else SLCANControl_Open
         )

       act handle
    )
    (sendC SLCANControl_Close)

sendSLCANMessage
  :: Handle
  -> SLCANMessage
  -> IO ()
sendSLCANMessage handle msg = do
  Control.Monad.void
    $ Data.ByteString.hPutStr
        handle
        $ Network.SLCAN.Builder.buildSLCANMessage
            msg
  System.IO.hFlush handle

sendSLCANControl
  :: Handle
  -> SLCANControl
  -> IO ()
sendSLCANControl h =
  sendSLCANMessage h
  . SLCANMessage_Control

recvSLCANMessage
  :: Handle
  -> IO (Either String SLCANMessage)
recvSLCANMessage handle = do
  Network.SLCAN.Parser.parseSLCANMessage
  <$> hGetTillCR handle

  where
    hGetTillCR h = do
      msg <-
        Data.ByteString.hGetSome
          h
          1024
      if Data.ByteString.Char8.last msg == '\r'
      then pure msg
      else hGetTillCR h >>= pure . (msg <>)

sendCANMessage
  :: Handle
  -> CANMessage
  -> IO ()
sendCANMessage h =
  sendSLCANMessage h
  . SLCANMessage_Data

newtype SLCANT m a = SLCANT
  { _unSLCANT :: ReaderT Handle m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader Handle
    , MonadIO
    , MonadUnliftIO
    )

instance MonadTrans SLCANT where
  lift = SLCANT . lift

-- | Run SLCANT transformer
runSLCANT
  :: Monad m
  => Handle
  -> SLCANT m a
  -> m a
runSLCANT sock =
    (`runReaderT` sock)
  . _unSLCANT

data SLCANException = SLCANException_ParseError String
    deriving Show

instance Exception SLCANException

runSLCAN
  :: ( MonadIO m
     , MonadUnliftIO m
     )
  => Handle
  -> SLCANConfig
  -> SLCANT m a
  -> m a
runSLCAN handle config act = do
  UnliftIO.withRunInIO $ \runInIO ->
    withSLCANHandle
      handle
      config
      (\h -> runInIO (runSLCANT h act))

instance MonadIO m => MonadCAN (SLCANT m) where
  send cm = do
    canSock <- ask
    liftIO $ sendCANMessage canSock cm
  recv = do
    canSock <- ask
    liftIO
      (recvSLCANMessage canSock)
    >>= \case
      Left e ->
        UnliftIO.throwIO $ SLCANException_ParseError e
      Right (SLCANMessage_Data cm) ->
        pure cm
      Right _other ->
        -- TODO: do something with
        -- SLCANMessage_Error
        -- and SLCANMessage_State
        -- like allow registering handlers for these
        -- or throwIO on _Error one
        recv
