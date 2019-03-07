{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
module RealWorld.Conduit.Client where

import           Control.Lens
import           Reflex

import           Data.Proxy                              (Proxy (Proxy))
import           Data.Text                               (Text)
import           Servant.Reflex                          (BaseUrl (BaseFullUrl),
                                                          Scheme (Http),
                                                          SupportsServantReflex)
import           Servant.Reflex.Multi                    (ClientMulti,
                                                          ReqResult, clientA)

import           RealWorld.Conduit.Api                   (Api, api)
import           RealWorld.Conduit.Api.Namespace         (Namespace)
import           RealWorld.Conduit.Api.Users.Account     (Account)
import           RealWorld.Conduit.Api.Users.Credentials (Credentials)


data UsersClient f t m = UsersClient
  { _usersLogin
    :: Dynamic t (f (Either Text (Namespace "user" Credentials)))
    -> Event t ()
    -> m (Event t (f (ReqResult () (Namespace "user" Account))))
  }
makeLenses ''UsersClient

data ApiClient f t m = ApiClient
  { _apiUsers :: UsersClient f t m
  }
makeLenses ''ApiClient

getClient
  :: forall f t m
  .  (Traversable f, Applicative f, SupportsServantReflex t m)
  => ApiClient f t m
getClient = ApiClient { .. } :: ApiClient f t m
  where
    bp :: Dynamic t BaseUrl
    bp = constDyn $ BaseFullUrl Http "localhost" 8080 "/"
    c :: ClientMulti t m Api f ()
    c = clientA api (Proxy :: Proxy m) (Proxy :: Proxy f) (Proxy :: Proxy ()) bp
    apiUsersC = c
    _apiUsers = UsersClient { .. }
      where
        _usersLogin = apiUsersC
