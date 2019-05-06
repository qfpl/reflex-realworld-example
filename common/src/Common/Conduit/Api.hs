{-# LANGUAGE DataKinds, DeriveGeneric, TypeOperators #-}
module Common.Conduit.Api
  ( module Common.Conduit.Api
  , module Articles
  , module Namespace
  , module Profiles
  , module User
  , module Users
  ) where

import Data.Proxy  (Proxy (..))
import Servant.API ((:<|>), (:>))

import Common.Conduit.Api.Articles  as Articles
import Common.Conduit.Api.Namespace as Namespace
import Common.Conduit.Api.Profiles  as Profiles
import Common.Conduit.Api.User      as User
import Common.Conduit.Api.Users     as Users

type Api token = "api" :> TopLevelApi token

type TopLevelApi token
     =    ("users"     :> UsersApi token)
     :<|> ("user"      :> UserApi token)
     :<|> ("articles"  :> ArticlesApi token)
     :<|> ("profiles"  :> ProfilesApi token)

api :: Proxy (Api token)
api = Proxy
