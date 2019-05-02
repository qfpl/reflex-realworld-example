{-# LANGUAGE DataKinds, DeriveGeneric, GeneralizedNewtypeDeriving #-}
module Backend.Conduit where

import Control.Monad.Reader  (ReaderT, runReaderT)
import Crypto.JOSE.JWK       (JWK)
import Data.Aeson            (FromJSON, ToJSON)
import GHC.Generics          (Generic)
import Servant               ((:<|>) ((:<|>)), Context ((:.), EmptyContext), Server)
import Servant.Auth.Server   (CookieSettings, FromJWT, JWTSettings, ToJWT, defaultCookieSettings,
                              defaultJWTSettings)
import Snap.Core             (Snap)

import Backend.Conduit.Database
import Common.Conduit.Api       (TopLevelApi)

data Claim = Claim { id :: Int } deriving Generic
instance ToJSON Claim
instance FromJSON Claim
instance ToJWT Claim
instance FromJWT Claim

data ConduitServerEnv = ConduitServerEnv

type ConduitServerM = ReaderT ConduitServerEnv Snap

runConduitServerM :: ConduitServerEnv -> ConduitServerM a -> Snap a
runConduitServerM e = flip runReaderT e

mkContext :: JWK -> Context '[CookieSettings, JWTSettings]
mkContext jwk = defaultCookieSettings :. defaultJWTSettings jwk :. EmptyContext

server :: Server (TopLevelApi Claim) '[] ConduitServerM
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
