{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module Common.Conduit.Api.Users where

import           Servant.API                             ((:<|>), (:>),
                                                          JSON, Post,
                                                          PostCreated, ReqBody)

import           Common.Conduit.Api.Namespace         (Namespace)
import           Common.Conduit.Api.User.Account     (Account)
import           Common.Conduit.Api.Users.Credentials (Credentials)
import           Common.Conduit.Api.Users.Registrant  (Registrant)

type UsersApi token =
  ( "login"
    :> ReqBody '[JSON] (Namespace "user" Credentials)
    :> Post '[JSON] (Namespace "user" Account)
  ) :<|> (
    ReqBody '[JSON] (Namespace "user" Registrant)
    :> PostCreated '[JSON] (Namespace "user" Account)
  )
