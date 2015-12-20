{-# LANGUAGE
    OverloadedStrings
  #-}

module Network.Wai.Session.TimeMap where

import Network.Wai.Session

import Data.UUID
import Data.UUID.V4   (nextRandom)
import Data.Hashable
import qualified Data.TimeMap           as TM
import qualified Data.ByteString        as BS

import Control.Monad.IO.Class
import Control.Concurrent.STM


uuidSessionCfg :: ( Hashable k
                  , Eq k
                  , MonadIO m
                  ) => (k -> BS.ByteString)       -- ^ render key
                    -> (BS.ByteString -> Maybe k) -- ^ parse key
                    -> BS.ByteString              -- ^ cookie name for key
                    -> BS.ByteString              -- ^ cookie name for UUID
                    -> TM.TimeMap k UUID          -- ^ session cache reference
                    -> SessionConfig m k UUID
uuidSessionCfg rKey pKey keyN valN cache =
  SessionConfig
    { renderKey = rKey
    , renderVal = toASCIIBytes
    , parseKey  = pKey
    , parseVal  = fromASCIIBytes
    , keyName   = keyN
    , valName   = valN
    , expire    = 3600
    , newVal    = updateSession
    }
  where
    updateSession sid nonce = do
      mOldNonce <- liftIO $ atomically (TM.lookup sid cache)
      case mOldNonce of
        Just oldNonce
          | oldNonce == nonce -> do
              newNonce <- liftIO nextRandom
              liftIO $ TM.adjust (const newNonce) sid cache
              return (Just newNonce)
        _ -> return Nothing
