{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module RealWorld.Conduit.Api.Users where

import           Servant.API                             ((:>), JSON, Post,
                                                          ReqBody)

import           RealWorld.Conduit.Api.Namespace         (Namespace)
import           RealWorld.Conduit.Api.Users.Account     (Account)
import           RealWorld.Conduit.Api.Users.Credentials (Credentials)

type UsersApi = "login"
  :> ReqBody '[JSON] (Namespace "user" Credentials)
  :> Post '[JSON] (Namespace "user" Account)
