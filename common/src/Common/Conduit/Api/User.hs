{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module Common.Conduit.Api.User where

import           Servant.API                        ((:<|>), (:>), Get, JSON,
                                                     Put, ReqBody)
import           Servant.Auth                       (Auth, JWT)

import           Common.Conduit.Api.Namespace    (Namespace)
import           Common.Conduit.Api.User.Account (Account)
import           Common.Conduit.Api.User.Update  (UpdateUser)

type UserApi token =
  (
    Auth '[JWT] token
    :> Get '[JSON] (Namespace "user" Account)
  ) :<|> (
    Auth '[JWT] token
    :> ReqBody '[JSON] (Namespace "user" UpdateUser)
    :> Put '[JSON] (Namespace "user" Account)
  )
