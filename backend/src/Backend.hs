{-# LANGUAGE DataKinds, DeriveGeneric, EmptyCase, FlexibleContexts, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms, RankNTypes, StandaloneDeriving, TypeFamilies, TypeOperators         #-}
module Backend where

import Control.Lens

import qualified Crypto.JOSE              as HOSE
import qualified Crypto.JOSE.Types        as HOSE
import Control.Monad.IO.Class (liftIO)
import           Data.Maybe               (maybe)
import           Data.Text                (Text)
import qualified Data.Text                as T
import           Data.Text.Encoding       (encodeUtf8)
import           Obelisk.Backend
import           Obelisk.ExecutableConfig (get)
import           Obelisk.Route
import           Servant                  (serveSnapWithContext)
import           SetCookieOrphan          ()

import Backend.Conduit    (jwtSettings, mkContext, mkEnv, runConduitServerM, server)
import Common.Conduit.Api (api)
import Common.Route       (BackendRoute (..), FrontendRoute, backendRouteEncoder)
import Backend.Conduit.Database (openConduitDb)

getYolo :: Text -> IO Text
getYolo l = maybe (error . T.unpack $ "Please fill in config: " <> l) T.strip <$> get l

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      pgConnStr <- getYolo "config/backend/pgConnStr"
      jwtKey    <- getYolo "config/backend/jwtKey"
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
