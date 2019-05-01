{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module RealWorld.Conduit.Api where

import           Data.Proxy                     (Proxy (..))
import           Servant.API                    ((:<|>), (:>))

import           RealWorld.Conduit.Api.Articles (ArticlesApi)
import           RealWorld.Conduit.Api.User     (UserApi)
import           RealWorld.Conduit.Api.Users    (UsersApi)
import           RealWorld.Conduit.Api.Profile  (ProfileApi)

type Api token = "api" :> TopLevelApi token

type TopLevelApi token
     =    ("users"     :> UsersApi token)
     :<|> ("user"      :> UserApi token)
     :<|> ("articles"  :> ArticlesApi token)
     :<|> ("profiles"  :> ProfileApi token)

api :: Proxy (Api token)
api = Proxy
