{-# LANGUAGE DataKinds, FlexibleContexts #-}
module Frontend.Conduit.Client where

import Control.Lens
import Reflex

import Data.Functor.Identity (Identity (..))
import Data.Proxy            (Proxy (Proxy))
import Data.Text             (Text)
import Servant.API           ((:<|>) ((:<|>)), (:>), NoContent)
import Servant.Auth          (Auth, JWT)
import Servant.Common.Req    (QParam, Req, headers)
import Servant.Reflex        (BaseUrl (BasePath), SupportsServantReflex)
import Servant.Reflex.Multi  (ClientMulti, HasClientMulti (..), ReqResult, clientA)

import Common.Conduit.Api (Api)

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
import Frontend.Conduit.Client.Internal

login
  :: SupportsServantReflex t m
  => Dynamic t (Either Text (Namespace "user" Credentials))
  -> Event t ()
  -> m (Event t (ReqResult () (Namespace "user" Account)))
login credDyn submitE = unIdF $
  getClient ^. apiUsers . usersLogin . fillIdF credDyn . fill submitE

register
  :: SupportsServantReflex t m
  => Dynamic t (Either Text (Namespace "user" Registrant))
  -> Event t ()
  -> m (Event t (ReqResult () (Namespace "user" Account)))
register regDyn submitE = unIdF $
  getClient ^. apiUsers . usersRegister . fillIdF regDyn . fill submitE

getCurrentUser
  :: SupportsServantReflex t m
  => Dynamic t (Maybe Token)
  -> Event t ()
  -> m (Event t (ReqResult () (Namespace "user" Account)))
getCurrentUser tokenDyn submitE = unIdF $
  getClient ^. apiUser . userCurrent . fillIdF tokenDyn . fill submitE

updateCurrentUser
  :: SupportsServantReflex t m
  => Dynamic t (Maybe Token)
  -> Dynamic t (Either Text (Namespace "user" UpdateUser))
  -> Event t ()
  -> m (Event t (ReqResult () (Namespace "user" Account)))
updateCurrentUser tokenDyn updateDyn submitE = unIdF $
  getClient ^. apiUser . userUpdate . fillIdF tokenDyn . fillIdF updateDyn . fill submitE

getProfile
  :: SupportsServantReflex t m
  => Dynamic t (Maybe Token)
  -> Dynamic t (Either Text Text)
  -> Event t ()
  -> m (Event t (ReqResult () (Namespace "profile" Profile)))
getProfile tokenDyn usernameDyn submitE = unIdF $
  getClient ^. apiProfile . profileGet . fillIdF tokenDyn . fillId usernameDyn . fill submitE

-- TODO FollowUser
-- TODO UnFollowUser

listArticles
  :: SupportsServantReflex t m
  => Dynamic t (Maybe Token)
  -> Dynamic t (QParam Integer)
  -> Dynamic t (QParam Integer)
  -> Dynamic t [Text]
  -> Dynamic t [Text]
  -> Dynamic t [Text]
  -> Event t ()
  -> m (Event t (ReqResult () Articles))
listArticles tokenDyn limitDyn offsetDyn authorsDyn favoritedsDyn tagsDyn submitE = unIdF $
  getClient ^. apiArticles . articlesList
    . fillIdF tokenDyn
    . fillIdF limitDyn
    . fillIdF offsetDyn
    . fillIdF authorsDyn
    . fillIdF favoritedsDyn
    . fillIdF tagsDyn
    . fill submitE

-- TODO Feed

getArticle
  :: SupportsServantReflex t m
  => Dynamic t (Maybe Token)
  -> Dynamic t (Either Text Text)
  -> Event t ()
  -> m (Event t (ReqResult () (Namespace "article" Article)))
getArticle tokenDyn slugDyn submitE = unIdF $
  getClient ^. apiArticles . articlesArticle . fillIdF tokenDyn . fillId slugDyn . articleGet . fill submitE

createArticle
  :: SupportsServantReflex t m
  => Dynamic t (Maybe Token)
  -> Dynamic t (Either Text (Namespace "article" CreateArticle))
  -> Event t ()
  -> m (Event t (ReqResult () (Namespace "article" Article)))
createArticle tokenDyn createDyn submitE = unIdF $
  getClient ^. apiArticles . articlesCreate . fillIdF tokenDyn . fillIdF createDyn . fill submitE

-- TODO Update Article
-- TODO Delete Article

createComment
  :: (SupportsServantReflex t m)
  => Dynamic t (Maybe Token)
  -> Dynamic t (Either Text Text)
  -> Dynamic t (Either Text (Namespace "comment" CreateComment))
  -> Event t ()
  -> m (Event t (ReqResult () (Namespace "comment" Comment)))
createComment tokenDyn slugDyn createDyn submitE = unIdF $
  getClient ^. apiArticles . articlesArticle
    . fillIdF tokenDyn
    . fillId slugDyn
    . articleCommentCreate
    . fillIdF createDyn
    . fill submitE

getComments
  :: (SupportsServantReflex t m)
  => Dynamic t (Maybe Token)
  -> Dynamic t (Either Text Text)
  -> Event t ()
  -> m (Event t (ReqResult () (Namespace "comments" [Comment])))
getComments tokenDyn slugDyn submitE = unIdF $
  getClient ^. apiArticles . articlesArticle
    . fillIdF tokenDyn
    . fillId slugDyn
    . articleComments
    . fill submitE

deleteComment
  :: (SupportsServantReflex t m)
  => Dynamic t (Maybe Token)
  -> Dynamic t (Either Text Text)
  -> Dynamic t (Either Text Int)
  -> Event t ()
  -> m (Event t (ReqResult () NoContent))
deleteComment tokenDyn slugDyn commentIdDyn submitE = unIdF $
  getClient ^. apiArticles . articlesArticle
    . fillIdF tokenDyn
    . fillId slugDyn
    . articleCommentDelete
    . fillId commentIdDyn
    . fill submitE

-- TODO Favorite / Unfavorite
-- TODO GetTags



unIdF :: (Reflex t, Functor m) => m (Event t (Identity a)) -> m (Event t a)
unIdF = fmap (fmap runIdentity)

idF :: Functor f => f a -> f (Identity a)
idF = fmap Identity

fillId :: a -> Getting f (Identity a -> b) b
fillId a = fill (Identity a)

fillIdF :: Functor f => f a -> Getting g (f (Identity a) -> b) b
fillIdF a = fill (idF a)
