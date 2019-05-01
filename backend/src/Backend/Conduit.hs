{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
module Backend.Conduit where

import           Crypto.JOSE.JWK       (JWK)
import qualified Crypto.JOSE.JWK       as JWK
import           Data.Aeson            (FromJSON, ToJSON)
import           Data.Text             (Text)
import           GHC.Generics          (Generic)
import           RealWorld.Conduit.Api
import           Servant               ((:<|>) ((:<|>)),
                                        Context ((:.), EmptyContext), Server,
                                        serveSnapWithContext)
import           Servant.Auth.Server   (CookieSettings, FromJWT, JWTSettings,
                                        ToJWT, defaultCookieSettings,
                                        defaultJWTSettings)
import           Snap.Core             (Snap, path)

data Claim = Claim { id :: Int } deriving Generic
instance ToJSON Claim
instance FromJSON Claim
instance ToJWT Claim
instance FromJWT Claim

data Environment = Environment
  { jwtSettings :: JWTSettings
  }

envToContext :: Environment -> Context '[CookieSettings, JWTSettings]
envToContext = undefined

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
