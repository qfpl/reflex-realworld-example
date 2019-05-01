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

import           Control.Monad.IO.Class   (liftIO)
import           Data.Proxy               (Proxy (..))
import           GHC.Generics             (Generic)
import           Obelisk.Backend
import           Obelisk.ExecutableConfig (get)
import           Obelisk.Route
import           Servant                  ((:<|>) ((:<|>)),
                                           Context ((:.), EmptyContext), Server,
                                           serveSnapWithContext)
import           Servant.Auth.Server      (CookieSettings, FromJWT, JWTSettings,
                                           ToJWT, defaultCookieSettings,
                                           defaultJWTSettings)
import           SetCookieOrphan          ()
import           Snap.Core                (path)

import           Backend.Conduit          (Claim, server, envToContext)
import           Common.Route
import           RealWorld.Conduit.Api

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> do
      let context = envToContext _
      serve $ \case
        (BackendRoute_Missing :/ ()) -> return ()
        (BackendRoute_Api     :/ ()) -> path "api" $
          serveSnapWithContext (Proxy :: Proxy (TopLevelApi Claim)) context server
  , _backend_routeEncoder = backendRouteEncoder
  }
