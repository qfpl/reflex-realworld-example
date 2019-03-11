{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module RealWorld.Conduit.Api.User where

import           Servant.API                         ((:<|>), (:>), Get, JSON,
                                                      ReqBody, Put)
import           Servant.Auth                        (Auth, JWT)

import           RealWorld.Conduit.Api.Namespace     (Namespace)
import           RealWorld.Conduit.Api.User.Update   (UpdateUser)
import           RealWorld.Conduit.Api.User.Account (Account)

type UserApi =
  (
    Auth '[JWT] Int
    :> Get '[JSON] (Namespace "user" Account)
  ) :<|> (
    Auth '[JWT] Int
    :> ReqBody '[JSON] (Namespace "user" UpdateUser)
    :> Put '[JSON] (Namespace "user" Account)
  )
