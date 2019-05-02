{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module Common.Conduit.Api where

import           Data.Proxy                     (Proxy (..))
import           Servant.API                    ((:<|>), (:>))

import           Common.Conduit.Api.Articles (ArticlesApi)
import           Common.Conduit.Api.User     (UserApi)
import           Common.Conduit.Api.Users    (UsersApi)
import           Common.Conduit.Api.Profile  (ProfileApi)

type Api token = "api" :> TopLevelApi token

type TopLevelApi token
     =    ("users"     :> UsersApi token)
     :<|> ("user"      :> UserApi token)
     :<|> ("articles"  :> ArticlesApi token)
     :<|> ("profiles"  :> ProfileApi token)

api :: Proxy (Api token)
api = Proxy
