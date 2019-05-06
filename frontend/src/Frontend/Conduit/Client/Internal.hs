{-# LANGUAGE DataKinds, FlexibleContexts, FlexibleInstances, MultiParamTypeClasses, OverloadedStrings   #-}
{-# LANGUAGE PolyKinds, RankNTypes, RecordWildCards, ScopedTypeVariables, TemplateHaskell, TypeFamilies #-}
{-# LANGUAGE TypeOperators                                                                              #-}
{-# OPTIONS_GHC -fno-warn-orphans  #-}
module Frontend.Conduit.Client.Internal where

import Control.Lens
import Reflex

import Control.Applicative  (liftA2)
import Data.Proxy           (Proxy (Proxy))
import Data.Text            (Text)
import Servant.API          ((:<|>) ((:<|>)), (:>), NoContent)
import Servant.Auth         (Auth, JWT)
import Servant.Common.Req   (QParam, Req, headers)
import Servant.Reflex       (BaseUrl (BasePath), SupportsServantReflex)
import Servant.Reflex.Multi (ClientMulti, HasClientMulti (..), ReqResult, clientA)

import Common.Conduit.Api                        (Api)

import Common.Conduit.Api.Articles.Article       (Article)
import Common.Conduit.Api.Articles.Articles      (Articles)
import Common.Conduit.Api.Articles.Attributes    (CreateArticle)
import Common.Conduit.Api.Articles.Comment       (Comment)
import Common.Conduit.Api.Articles.CreateComment (CreateComment)
import Common.Conduit.Api.Namespace              (Namespace)
import Common.Conduit.Api.Profiles               (Profile)
import Common.Conduit.Api.User.Account           (Account, Token, getToken)
import Common.Conduit.Api.User.Update            (UpdateUser)
import Common.Conduit.Api.Users.Credentials      (Credentials)
import Common.Conduit.Api.Users.Registrant       (Registrant)

fill :: a -> Getting f (a -> b) b
fill a = to ($ a)

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
    :: Dynamic t (f (Maybe Token))
    -> Event t ()
    -> m (Event t (f (ReqResult () (Namespace "user" Account))))
  , _userUpdate
    :: Dynamic t (f (Maybe Token))
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
    :: Dynamic t (f (Either Text (Namespace "comment" CreateComment)))
    -> Event t ()
    -> m (Event t (f (ReqResult () (Namespace "comment" Comment))))
  , _articleCommentDelete
    :: f (Dynamic t (Either Text Int))
    -> Event t ()
    -> m (Event t (f (ReqResult () NoContent)))
  }
makeLenses ''ArticleClient

data ArticlesClient f t m = ArticlesClient
  { _articlesList
    :: Dynamic t (f (Maybe Token))
    -> Dynamic t (f (QParam Integer))
    -> Dynamic t (f (QParam Integer))
    -> Dynamic t (f [Text])
    -> Dynamic t (f [Text])
    -> Dynamic t (f [Text])
    -> Event t ()
    -> m (Event t (f (ReqResult () Articles)))
  , _articlesCreate
    :: Dynamic t (f (Maybe Token))
    -> Dynamic t (f (Either Text (Namespace "article" CreateArticle)))
    -> Event t ()
    -> m (Event t (f (ReqResult () (Namespace "article" Article))))
  , _articlesArticle :: Dynamic t (f (Maybe Token)) -> f (Dynamic t (Either Text Text)) -> ArticleClient f t m
  }
makeLenses ''ArticlesClient

data ProfileClient f t m = ProfileClient
  { _profileGet
    :: Dynamic t (f (Maybe Token))
    -> (f (Dynamic t (Either Text Text)))
    -> Event t ()
    -> m (Event t (f (ReqResult () (Namespace "profile" Profile))))
  }
makeLenses ''ProfileClient

data ApiClient f t m = ApiClient
  { _apiUsers    :: UsersClient f t m
  , _apiUser     :: UserClient f t m
  , _apiArticles :: ArticlesClient f t m
  , _apiProfile  :: ProfileClient f t m
  }
makeLenses ''ApiClient

getClient
  :: forall f t m
  .  (Traversable f, Applicative f, SupportsServantReflex t m)
  => ApiClient f t m
getClient = mkClient (pure $ BasePath "/") -- This would be much better if there was a RouteToUrl BackendRoute
  where
    mkClient bp = ApiClient { .. } :: ApiClient f t m
      where
        c :: ClientMulti t m (Api Token) f ()
        c = clientA (Proxy :: Proxy (Api Token))  (Proxy :: Proxy m) (Proxy :: Proxy f) (Proxy :: Proxy ()) bp
        apiUsersC :<|> apiUserC :<|> apiArticlesC :<|> apiProfileC = c
        _apiUsers = UsersClient { .. }
          where
            _usersLogin :<|> _usersRegister = apiUsersC
        _apiUser = UserClient { .. }
          where
            _userCurrent :<|> _userUpdate = apiUserC
        _apiArticles = ArticlesClient { .. }
          where
            _articlesList :<|> _articlesCreate :<|> articleC = apiArticlesC
            _articlesArticle auth slug = ArticleClient { .. }
              where
                _articleGet  :<|> _articleComments :<|> _articleCommentCreate :<|> _articleCommentDelete = articleC auth slug
        _apiProfile = ProfileClient { .. }
          where
            _profileGet = apiProfileC

-- TODO : Make this not dodgy and put it in servant-reflex.
-- It's dumb, because servant-reflex has an instance for the normal client that ignores any auth tokens.
-- So we use multi here specifically so I don't have to fork servant reflex given it's easy to add and remove
-- the Identity functors in the client functions.
instance (HasClientMulti t m api f tag, Reflex t, Applicative f)
  => HasClientMulti t m (Auth '[JWT] Token :> api) f tag where
  type ClientMulti t m (Auth '[JWT] Token :> api) f tag =
    Dynamic t (f (Maybe Token)) -> ClientMulti t m api f tag

  clientWithRouteMulti Proxy q f t reqs baseurl opts authdatas =
    clientWithRouteMulti (Proxy :: Proxy api) q f t reqs' baseurl opts
    where
      req' :: (Maybe Token) -> Req t -> Req t
      req' Nothing  r = r
      req' (Just a) r = r
        { headers = ( "Authorization" , constDyn . pure . ("Bearer " <>) . getToken $ a ) : (headers r)
        }
      reqs' = liftA2 req' <$> authdatas <*> reqs
