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
import           Obelisk.ExecutableConfig.Backend (HasBackendConfigs, getBackendConfig)
import           Obelisk.Route
import           Servant                          (serveSnapWithContext)
import           SetCookieOrphan                  ()

import Backend.Conduit          (jwtSettings, mkContext, mkEnv, runConduitServerM, server)
import Backend.Conduit.Database (openConduitDb)
import Common.Conduit.Api       (api)
import Common.Route             (BackendRoute (..), FrontendRoute, backendRouteEncoder)

getYolo :: HasBackendConfigs m => Text -> m Text
getYolo l = maybe (error . unpack $ "Please fill in config: config/backend/" <> l) T.strip <$> getBackendConfig l

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      pgConnStr <- getYolo "pgConnStr"
      jwtKey    <- getYolo "jwtKey"
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
