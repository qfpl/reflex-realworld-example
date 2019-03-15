{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PolyKinds             #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE TypeOperators         #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module RealWorld.Conduit.Client where

import           Control.Lens
import           Reflex

import           Control.Applicative                          (liftA2)
import           Data.Proxy                                   (Proxy (Proxy))
import           Data.Text                                    (Text)
import           Servant.API                                  ((:<|>) ((:<|>)),
                                                               (:>), NoContent)
import           Servant.Auth                                 (Auth, JWT)
import           Servant.Common.Req                           (QParam, Req,
                                                               headers)
import           Servant.Reflex                               (BaseUrl (BaseFullUrl),
                                                               Scheme (..),
                                                               SupportsServantReflex)
import           Servant.Reflex.Multi                         (ClientMulti, HasClientMulti (..),
                                                               ReqResult,
                                                               clientA)

import           RealWorld.Conduit.Api                        (Api, api)
import           RealWorld.Conduit.Api.Articles.Article       (Article)
import           RealWorld.Conduit.Api.Articles.Articles      (Articles)
import           RealWorld.Conduit.Api.Articles.Attributes    (CreateArticle)
import           RealWorld.Conduit.Api.Articles.Comment       (Comment)
import           RealWorld.Conduit.Api.Articles.CreateComment (CreateComment)
import           RealWorld.Conduit.Api.Namespace              (Namespace)
import           RealWorld.Conduit.Api.User.Account           (Account)
import           RealWorld.Conduit.Api.User.Update            (UpdateUser)
import           RealWorld.Conduit.Api.Users.Credentials      (Credentials)
import           RealWorld.Conduit.Api.Users.Registrant       (Registrant)

data UsersClient f t m = UsersClient
  { _usersLogin
    :: Dynamic t (f (Either Text (Namespace "user" Credentials)))
    -> Event t ()
    -> m (Event t (f (ReqResult () (Namespace "user" Account))))
  , _usersRegister
    :: Dynamic t (f (Either Text (Namespace "user" Registrant)))
    -> Event t ()
    -> m (Event t (f (ReqResult () (Namespace "user" Account))))
  }
makeLenses ''UsersClient

data UserClient f t m = UserClient
  { _userCurrent
    :: Dynamic t (f (Maybe Text))
    -> Event t ()
    -> m (Event t (f (ReqResult () (Namespace "user" Account))))
  , _userUpdate
    :: Dynamic t (f (Maybe Text))
    -> Dynamic t (f (Either Text (Namespace "user" UpdateUser)))
    -> Event t ()
    -> m (Event t (f (ReqResult () (Namespace "user" Account))))
  }
makeLenses ''UserClient

data ArticleClient f t m = ArticleClient
  { _articleGet
    :: Event t ()
    -> m (Event t (f (ReqResult () (Namespace "article" Article))))
  , _articleComments
    :: Event t ()
    -> m (Event t (f (ReqResult () (Namespace "comments" [Comment]))))
  , _articleCommentCreate
    :: Dynamic t (f (Maybe Text))
    -> Dynamic t (f (Either Text (Namespace "comment" CreateComment)))
    -> Event t ()
    -> m (Event t (f (ReqResult () (Namespace "comment" Comment))))
  , _articleCommentDelete
    :: f (Dynamic t (Either Text Int))
    -> Dynamic t (f (Maybe Text))
    -> Event t ()
    -> m (Event t (f (ReqResult () NoContent)))
  }
makeLenses ''ArticleClient

data ArticlesClient f t m = ArticlesClient
  { _articlesList
    :: Dynamic t (f (QParam Integer))
    -> Dynamic t (f (QParam Integer))
    -> Dynamic t (f [Text])
    -> Dynamic t (f [Text])
    -> Dynamic t (f [Text])
    -> Dynamic t (f (Maybe Text))
    -> Event t ()
    -> m (Event t (f (ReqResult () Articles)))
  , _articlesCreate
    :: Dynamic t (f (Maybe Text))
    -> Dynamic t (f (Either Text (Namespace "article" CreateArticle)))
    -> Event t ()
    -> m (Event t (f (ReqResult () (Namespace "article" Article))))
  , _articlesArticle :: f (Dynamic t (Either Text Text)) -> ArticleClient f t m
  }
makeLenses ''ArticlesClient

data ApiClient f t m = ApiClient
  { _apiUsers    :: UsersClient f t m
  , _apiUser     :: UserClient f t m
  , _apiArticles :: ArticlesClient f t m
  }
makeLenses ''ApiClient

-- Don't try this yet. We aren't exactly to spec properly yet
baseUrl :: BaseUrl
tokenName :: Text
--(baseUrl, tokenName) = (BaseFullUrl Https "conduit.productionready.io" 443 "/", "Token")
(baseUrl, tokenName) = (BaseFullUrl Http "localhost" 8080 "/", "Bearer")

getClient
  :: forall f t m
  .  (Traversable f, Applicative f, SupportsServantReflex t m)
  => ApiClient f t m
getClient = ApiClient { .. } :: ApiClient f t m
  where
    bp :: Dynamic t BaseUrl
    bp = constDyn $ baseUrl
    c :: ClientMulti t m Api f ()
    c = clientA api (Proxy :: Proxy m) (Proxy :: Proxy f) (Proxy :: Proxy ()) bp
    apiUsersC :<|> apiUserC :<|> apiArticlesC = c
    _apiUsers = UsersClient { .. }
      where
        _usersLogin :<|> _usersRegister = apiUsersC
    _apiUser = UserClient { .. }
      where
        _userCurrent :<|> _userUpdate = apiUserC
    _apiArticles = ArticlesClient { .. }
      where
        _articlesList :<|> _articlesCreate :<|> articleC = apiArticlesC
        _articlesArticle slug = ArticleClient { .. }
          where
            _articleGet  :<|> _articleComments :<|> _articleCommentCreate :<|> _articleCommentDelete = articleC slug

-- TODO : Move this to servant Auth after some tidy up
instance (HasClientMulti t m api f tag, Reflex t, Applicative f)
  => HasClientMulti t m (Auth '[JWT] a :> api) f tag where
  type ClientMulti t m (Auth '[JWT] a :> api) f tag =
    Dynamic t (f (Maybe Text)) -> ClientMulti t m api f tag

  clientWithRouteMulti Proxy q f t reqs baseurl opts authdatas =
    clientWithRouteMulti (Proxy :: Proxy api) q f t reqs' baseurl opts
    where
      req' :: (Maybe Text) -> Req t -> Req t
      req' Nothing  r = r
      req' (Just a) r = r
        { headers = ( "Authorization" , constDyn . pure $ tokenName <> " " <> a) : (headers r)
        }
      reqs' = liftA2 req' <$> authdatas <*> reqs
