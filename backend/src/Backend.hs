{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE EmptyCase          #-}
{-# LANGUAGE FlexibleContexts   #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE PatternSynonyms    #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}
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
import           Snap.Core                (path)

import Backend.Conduit       (Claim, mkContext, server)
import Common.Route
import RealWorld.Conduit.Api

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      pgConnStrMay <- get "config/backend/pgConnStr"
      jwtKeyMay    <- get "config/backend/jwtKey"
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
        (BackendRoute_Api     :/ ()) -> path "api" $
          serveSnapWithContext (Proxy :: Proxy (TopLevelApi Claim)) context server
  , _backend_routeEncoder = backendRouteEncoder
  }
