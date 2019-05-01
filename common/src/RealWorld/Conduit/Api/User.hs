{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module RealWorld.Conduit.Api.User where

import           Servant.API                        ((:<|>), (:>), Get, JSON,
                                                     Put, ReqBody)
import           Servant.Auth                       (Auth, JWT)

import           RealWorld.Conduit.Api.Namespace    (Namespace)
import           RealWorld.Conduit.Api.User.Account (Account)
import           RealWorld.Conduit.Api.User.Update  (UpdateUser)

type UserApi token =
  (
    Auth '[JWT] token
    :> Get '[JSON] (Namespace "user" Account)
  ) :<|> (
    Auth '[JWT] token
    :> ReqBody '[JSON] (Namespace "user" UpdateUser)
    :> Put '[JSON] (Namespace "user" Account)
  )
