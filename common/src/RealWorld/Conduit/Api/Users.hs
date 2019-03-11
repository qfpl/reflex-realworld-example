{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module RealWorld.Conduit.Api.Users where

import           Servant.API                             ((:<|>), (:>),
                                                          JSON, Post,
                                                          PostCreated, ReqBody)

import           RealWorld.Conduit.Api.Namespace         (Namespace)
import           RealWorld.Conduit.Api.User.Account     (Account)
import           RealWorld.Conduit.Api.Users.Credentials (Credentials)
import           RealWorld.Conduit.Api.Users.Registrant  (Registrant)

type UsersApi =
  ( "login"
    :> ReqBody '[JSON] (Namespace "user" Credentials)
    :> Post '[JSON] (Namespace "user" Account)
  ) :<|> (
    ReqBody '[JSON] (Namespace "user" Registrant)
    :> PostCreated '[JSON] (Namespace "user" Account)
  )
