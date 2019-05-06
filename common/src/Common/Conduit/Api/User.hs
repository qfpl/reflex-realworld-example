{-# LANGUAGE DataKinds, DeriveGeneric, TypeOperators #-}
module Common.Conduit.Api.User
  ( UserApi
  , module Namespace
  , module Account
  , module UpdateUser
  ) where

import Servant.API  ((:<|>), (:>), Get, JSON, Put, ReqBody)
import Servant.Auth (Auth, JWT)

import Common.Conduit.Api.Namespace    as Namespace (Namespace (Namespace))
import Common.Conduit.Api.User.Account as Account (Account (Account), Token (Token))
import Common.Conduit.Api.User.Update  as UpdateUser (UpdateUser (UpdateUser))

type UserApi token =
  (
    Auth '[JWT] token
    :> Get '[JSON] (Namespace "user" Account)
  ) :<|> (
    Auth '[JWT] token
    :> ReqBody '[JSON] (Namespace "user" UpdateUser)
    :> Put '[JSON] (Namespace "user" Account)
  )
