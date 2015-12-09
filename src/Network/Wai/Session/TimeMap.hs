{-# LANGUAGE
    OverloadedStrings
  #-}

module Network.Wai.Session.TimeMap where

import Network.Wai.Session

import Data.UUID
import Data.UUID.V4   (nextRandom)
import Data.Hashable
import qualified Data.TimeMap           as TM
import qualified Data.Vault.Lazy        as V
import qualified Data.ByteString        as BS



uuidSessionCfg :: ( Hashable k
                  , Eq k
                  ) => (k -> BS.ByteString)       -- ^ render key
                    -> (BS.ByteString -> Maybe k) -- ^ parse key
                    -> BS.ByteString              -- ^ cookie name for key
                    -> BS.ByteString              -- ^ cookie name for UUID
                    -> V.Key k                    -- ^ vault variable to store successful session lookups
                    -> TM.TimeMap k UUID          -- ^ session cache reference
                    -> SessionConfig IO k UUID
uuidSessionCfg rKey pKey keyN valN vKey cache =
  SessionConfig
    { renderKey = rKey
    , renderVal = toASCIIBytes
    , parseKey  = pKey
    , parseVal  = fromASCIIBytes
    , keyName   = keyN
    , valName   = valN
    , expire    = 3600
    , newVal    = updateSession
    , vaultVar  = vKey
    }
  where
    updateSession sid nonce = do
      mOldNonce <- TM.lookup sid cache
      case mOldNonce of
        Just oldNonce
          | oldNonce == nonce -> do
              new <- nextRandom
              TM.adjust (const new) sid cache
              return (Just nonce)
        _ -> return Nothing
