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
import           Crypto.JOSE.JWK          (JWK)
import qualified Crypto.JOSE.JWK          as JWK
import           Data.Aeson               (FromJSON, ToJSON)
import           Data.Proxy               (Proxy (..))
import           GHC.Generics             (Generic)
import           Obelisk.Backend
import           Obelisk.ExecutableConfig (get)
import           Obelisk.Route
import           Servant                  ((:<|>) ((:<|>)), Context ((:.), EmptyContext), Server,
                                           serveSnapWithContext)
import           Servant.Auth.Server      (CookieSettings, FromJWT, JWTSettings, ToJWT, defaultCookieSettings,
                                           defaultJWTSettings)
import           SetCookieOrphan          ()
import           Snap.Core                (Snap, path)

import Common.Route
import RealWorld.Conduit.Api

data Claim = Claim { id :: Int } deriving Generic
instance ToJSON Claim
instance FromJSON Claim
instance ToJWT Claim
instance FromJWT Claim

data Environment = Environment
  { jwtSettings :: JWTSettings
  }

mkEnv :: JWK -> Environment
mkEnv jwk = Environment
  { jwtSettings = defaultJWTSettings jwk
  }

server :: Server (TopLevelApi Claim) '[] Snap
server = usersServer :<|> userServer :<|> articlesServer :<|> profilesServer
  where
    usersServer = loginServer :<|> registerServer
      where
        loginServer _ = pure undefined
        registerServer _ = pure undefined
    userServer = currentUserServer :<|> updateUserServer
      where
        currentUserServer = undefined
        updateUserServer = undefined
    articlesServer = listArticlesServer :<|> getArticleServer
      where
        listArticlesServer = undefined
        getArticleServer = undefined
    profilesServer _ = undefined

context :: Environment -> Context '[CookieSettings, JWTSettings]
context handle =
  defaultCookieSettings :. jwtSettings handle :. EmptyContext

backend :: Backend BackendRoute FrontendRoute
backend = Backend
  { _backend_run = \serve -> serve $ \case
      (BackendRoute_Missing :/ ()) -> return ()
      (BackendRoute_Api     :/ ()) -> path "api" $ do
        pgUri  <- liftIO $ get "config/backend/postgresUri"
        secret <- liftIO $ get "config/backend/secret"
        jwk <- liftIO $ do
          putStrLn "PGURI"
          print pgUri
          putStrLn "secret"
          print secret
          JWK.genJWK (JWK.OctGenParam 256)
        serveSnapWithContext (Proxy :: Proxy (TopLevelApi Claim)) (context $ mkEnv jwk) server
  , _backend_routeEncoder = backendRouteEncoder
  }
