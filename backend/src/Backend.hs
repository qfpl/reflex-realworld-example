{-# LANGUAGE DataKinds, DeriveGeneric, EmptyCase, FlexibleContexts, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms, RankNTypes, StandaloneDeriving, TypeFamilies, TypeOperators         #-}
module Backend where

import Control.Lens

import           Control.Monad.IO.Class           (liftIO)
import qualified Crypto.JOSE                      as HOSE
import qualified Crypto.JOSE.Types                as HOSE
import           Data.Maybe                       (maybe)
import           Data.Text                        (Text, unpack)
import qualified Data.Text                        as T
import           Data.Text.Encoding               (encodeUtf8)
import           Obelisk.Backend
import           Obelisk.Route
import           Servant                          (serveSnapWithContext)

import Backend.Conduit          (jwtSettings, mkContext, mkEnv, runConduitServerM, server)
import Backend.Conduit.Database (openConduitDb)
import Common.Conduit.Api       (api)
import Common.Route             (BackendRoute (..), FrontendRoute, backendRouteEncoder)
import qualified Obelisk.ExecutableConfig.Lookup as ExeCfg
import Data.Map as Map
import qualified Data.ByteString as BS
import Data.Text.Encoding (decodeUtf8)

getYolo :: Text -> Map Text BS.ByteString -> Text
getYolo l cfgs = maybe (error . T.unpack $ "Please fill in config: config/" <> l) (T.strip . decodeUtf8) (Map.lookup l cfgs)

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      cfgs <- liftIO ExeCfg.getConfigs
      let jwtKey = getYolo "backend/jwtKey" cfgs
      let pgConnStr = getYolo "backend/pgConnStr" cfgs
      let jwk   =
               HOSE.fromKeyMaterial
             . HOSE.OctKeyMaterial
             . HOSE.OctKeyParameters
             . HOSE.Base64Octets
             . encodeUtf8
             $  jwtKey
      env <- mkEnv pgConnStr jwk
      let context = mkContext (env ^. jwtSettings)
      liftIO $ putStrLn "About to test the db connection. If ob run dies, check out config/backend/pgConnStr"
      _ <- openConduitDb (encodeUtf8 pgConnStr)
      serve $ \case
        (BackendRoute_Missing :/ ()) -> return ()
        (BackendRoute_Api     :/ _)  -> runConduitServerM env $ serveSnapWithContext api context server
  , _backend_routeEncoder = backendRouteEncoder
  }
