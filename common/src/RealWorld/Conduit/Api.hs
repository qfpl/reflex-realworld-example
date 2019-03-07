{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module RealWorld.Conduit.Api where

import           Data.Proxy                  (Proxy (..))
import           Servant.API                 ((:>))

import           RealWorld.Conduit.Api.Users (UsersApi)

type Api = "api" :> ("users" :> UsersApi)

api :: Proxy Api
api = Proxy
