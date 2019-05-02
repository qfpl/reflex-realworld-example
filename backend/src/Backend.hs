{-# LANGUAGE DataKinds, DeriveGeneric, EmptyCase, FlexibleContexts, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms, RankNTypes, StandaloneDeriving, TypeFamilies, TypeOperators         #-}
module Backend where

import qualified Crypto.JOSE              as HOSE
import qualified Crypto.JOSE.Types        as HOSE
import           Data.Maybe               (fromJust)
import           Data.Proxy               (Proxy (..))
import           Data.Text.Encoding       (encodeUtf8)
import           Obelisk.Backend
import           Obelisk.ExecutableConfig (get)
import           Obelisk.Route
import           Servant                  (serveSnapWithContext)
import           SetCookieOrphan          ()
import           Snap.Core                (dir)

import Backend.Conduit    (Claim, ConduitServerEnv (..), mkContext, runConduitServerM, server)
import Common.Conduit.Api (TopLevelApi)
import Common.Route       (BackendRoute (..), FrontendRoute, backendRouteEncoder)

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      pgConnStrMay <- get "config/backend/pgConnStr"
      jwtKeyMay    <- get "config/backend/jwtKey"
      let env      = ConduitServerEnv
      let jwtMay   =
            (  HOSE.fromKeyMaterial
             . HOSE.OctKeyMaterial
             . HOSE.OctKeyParameters
             . HOSE.Base64Octets
             . encodeUtf8
            ) <$> jwtKeyMay
      let context = mkContext (fromJust jwtMay)
      serve $ \case
        (BackendRoute_Missing :/ ()) -> return ()
        (BackendRoute_Api     :/ _) ->
          let apiProxy = Proxy :: Proxy (TopLevelApi Claim)
          in dir "api" $ runConduitServerM env $ serveSnapWithContext apiProxy context server
  , _backend_routeEncoder = backendRouteEncoder
  }
