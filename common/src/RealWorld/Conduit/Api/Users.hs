{-# LANGUAGE DataKinds     #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
module RealWorld.Conduit.Api.Users where

import           Servant.API                             ((:>), JSON, Post,
                                                          ReqBody)

import           RealWorld.Conduit.Api.Users.Account     (Account)
import           RealWorld.Conduit.Api.Users.Credentials (Credentials)

type UsersApi = "login" :> ReqBody '[JSON] Credentials :> Post '[JSON] Account
