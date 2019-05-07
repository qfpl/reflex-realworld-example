{-# LANGUAGE DataKinds, FlexibleContexts, ScopedTypeVariables #-}
module Frontend.Conduit.Client where

import Control.Lens
import Reflex
import Reflex.Dom.Core hiding (Namespace)

import Data.Aeson              (decode)
import Data.Functor.Identity   (Identity (..))
import Data.Text               (Text)
import Data.Text.Lazy          (fromStrict)
import Data.Text.Lazy.Encoding (encodeUtf8)
import Reflex.Dom.Xhr          (xhrResponse_responseText, xhrResponse_status)
import Servant.API             (NoContent)
import Servant.Common.Req      (QParam)
import Servant.Reflex.Multi    (ReqResult (ResponseFailure), reqSuccess)

import           Common.Conduit.Api.Articles.Article       (Article)
import           Common.Conduit.Api.Articles.Articles      (Articles)
import           Common.Conduit.Api.Articles.Attributes    (CreateArticle)
import           Common.Conduit.Api.Articles.Comment       (Comment)
import           Common.Conduit.Api.Articles.CreateComment (CreateComment)
import           Common.Conduit.Api.Errors                 (ErrorBody)
import           Common.Conduit.Api.Namespace              (Namespace)
import           Common.Conduit.Api.Profiles               (Profile)
import           Common.Conduit.Api.User.Account           (Account, Token)
import           Common.Conduit.Api.User.Update            (UpdateUser)
import           Common.Conduit.Api.Users.Credentials      (Credentials)
import           Common.Conduit.Api.Users.Registrant       (Registrant)
import           Common.Conduit.Api.Validation             (ValidationErrors)
import           Frontend.Conduit.Client.Internal

type ClientRes t a = (Event t a, Event t ClientError, Dynamic t Bool)

data ClientError
  = Forbidden
  | NotFound
  | Unauthorised
  | FailedValidation (Maybe (ErrorBody ValidationErrors))
  | OtherError Word Text
  deriving (Show)

login
  :: (Reflex t, Applicative m, Prerender js t m)
  => Dynamic t (Either Text (Namespace "user" Credentials))
  -> Event t ()
  -> m (ClientRes t (Namespace "user" Account))
login credDyn submitE = fmap switchClientRes $ prerender (pure emptyClientRes) $ do
  resE <- unIdF $ getClient ^. apiUsers . usersLogin . fillIdF credDyn . fill submitE
  let successE = fmapMaybe reqSuccess resE
  let errorE   = fmapMaybe reqClientError resE
  submittingDyn <- holdDyn False $ leftmost [True <$ submitE, False <$ errorE, False <$ successE]

  pure (successE, errorE, submittingDyn)

register
  :: (Reflex t, Applicative m, Prerender js t m)
  => Dynamic t (Either Text (Namespace "user" Registrant))
  -> Event t ()
  -> m (Event t (ReqResult () (Namespace "user" Account)))
register regDyn submitE = fmap switchDyn $ prerender (pure never) $ unIdF $
  getClient ^. apiUsers . usersRegister . fillIdF regDyn . fill submitE

getCurrentUser
  :: (Reflex t, Applicative m, Prerender js t m)
  => Dynamic t (Maybe Token)
  -> Event t ()
  -> m (Event t (ReqResult () (Namespace "user" Account)))
getCurrentUser tokenDyn submitE = fmap switchDyn $ prerender (pure never) $ unIdF $
  getClient ^. apiUser . userCurrent . fillIdF tokenDyn . fill submitE

updateCurrentUser
  :: (Reflex t, Applicative m, Prerender js t m)
  => Dynamic t (Maybe Token)
  -> Dynamic t (Either Text (Namespace "user" UpdateUser))
  -> Event t ()
  -> m (Event t (ReqResult () (Namespace "user" Account)))
updateCurrentUser tokenDyn updateDyn submitE = fmap switchDyn $ prerender (pure never) $ unIdF $
  getClient ^. apiUser . userUpdate . fillIdF tokenDyn . fillIdF updateDyn . fill submitE

getProfile
  :: (Reflex t, Applicative m, Prerender js t m)
  => Dynamic t (Maybe Token)
  -> Dynamic t (Either Text Text)
  -> Event t ()
  -> m (Event t (ReqResult () (Namespace "profile" Profile)))
getProfile tokenDyn usernameDyn submitE = fmap switchDyn $ prerender (pure never) $ unIdF $
  getClient ^. apiProfile . profileGet . fillIdF tokenDyn . fillId usernameDyn . fill submitE

-- TODO FollowUser
-- TODO UnFollowUser

listArticles
  :: (Reflex t, Applicative m, Prerender js t m)
  => Dynamic t (Maybe Token)
  -> Dynamic t (QParam Integer)
  -> Dynamic t (QParam Integer)
  -> Dynamic t [Text]
  -> Dynamic t [Text]
  -> Dynamic t [Text]
  -> Event t ()
  -> m (Event t (ReqResult () Articles))
listArticles tokenDyn limitDyn offsetDyn authorsDyn favoritedsDyn tagsDyn submitE =
  fmap switchDyn $ prerender (pure never) $ unIdF $
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
  :: (Reflex t, Applicative m, Prerender js t m)
  => Dynamic t (Maybe Token)
  -> Dynamic t (Either Text Text)
  -> Event t ()
  -> m (Event t (ReqResult () (Namespace "article" Article)))
getArticle tokenDyn slugDyn submitE = fmap switchDyn $ prerender (pure never) $ unIdF $
  getClient ^. apiArticles . articlesArticle . fillIdF tokenDyn . fillId slugDyn . articleGet . fill submitE

createArticle
  :: (Reflex t, Applicative m, Prerender js t m)
  => Dynamic t (Maybe Token)
  -> Dynamic t (Either Text (Namespace "article" CreateArticle))
  -> Event t ()
  -> m (Event t (ReqResult () (Namespace "article" Article)))
createArticle tokenDyn createDyn submitE = fmap switchDyn $ prerender (pure never) $ unIdF $
  getClient ^. apiArticles . articlesCreate . fillIdF tokenDyn . fillIdF createDyn . fill submitE

-- TODO Update Article
-- TODO Delete Article

createComment
  :: (Reflex t, Applicative m, Prerender js t m)
  => Dynamic t (Maybe Token)
  -> Dynamic t (Either Text Text)
  -> Dynamic t (Either Text (Namespace "comment" CreateComment))
  -> Event t ()
  -> m (Event t (ReqResult () (Namespace "comment" Comment)))
createComment tokenDyn slugDyn createDyn submitE = fmap switchDyn $ prerender (pure never) $ unIdF $
  getClient ^. apiArticles . articlesArticle
    . fillIdF tokenDyn
    . fillId slugDyn
    . articleCommentCreate
    . fillIdF createDyn
    . fill submitE

getComments
  :: (Reflex t, Applicative m, Prerender js t m)
  => Dynamic t (Maybe Token)
  -> Dynamic t (Either Text Text)
  -> Event t ()
  -> m (Event t (ReqResult () (Namespace "comments" [Comment])))
getComments tokenDyn slugDyn submitE = fmap switchDyn $ prerender (pure never) $ unIdF $
  getClient ^. apiArticles . articlesArticle
    . fillIdF tokenDyn
    . fillId slugDyn
    . articleComments
    . fill submitE

deleteComment
  :: (Reflex t, Applicative m, Prerender js t m)
  => Dynamic t (Maybe Token)
  -> Dynamic t (Either Text Text)
  -> Dynamic t (Either Text Int)
  -> Event t ()
  -> m (Event t (ReqResult () NoContent))
deleteComment tokenDyn slugDyn commentIdDyn submitE = fmap switchDyn $ prerender (pure never) $ unIdF $
  getClient ^. apiArticles . articlesArticle
    . fillIdF tokenDyn
    . fillId slugDyn
    . articleCommentDelete
    . fillId commentIdDyn
    . fill submitE

-- TODO Favorite / Unfavorite
-- TODO GetTags

-- Helpers ---------------------------------------------------------------------------------------------------

emptyClientRes :: Reflex t => (Event t a, Event t ClientError, Dynamic t Bool)
emptyClientRes = (never, never, constDyn False)

reqClientError :: ReqResult tag a -> Maybe ClientError
reqClientError (ResponseFailure _ msg xhrR) = Just $ case view xhrResponse_status xhrR of
  401 -> Unauthorised
  403 -> Forbidden
  404 -> NotFound
  422 -> FailedValidation (xhrR ^? xhrResponse_responseText . _Just . to fromStrict . to encodeUtf8 . to decode . _Just)
  w   -> OtherError w msg
reqClientError _                              = Nothing

switchClientRes
  :: Reflex t
  => Dynamic t (Event t a, Event t b, Dynamic t d)
  -> (Event t a, Event t b, Dynamic t d)
switchClientRes d =
  ( switchDyn . fmap (^. _1) $ d
  , switchDyn . fmap (^. _2) $ d
  , d >>= (^. _3)
  )

unIdF :: (Reflex t, Functor m) => m (Event t (Identity a)) -> m (Event t a)
unIdF = fmap (fmap runIdentity)

idF :: Functor f => f a -> f (Identity a)
idF = fmap Identity

fillId :: a -> Getting f (Identity a -> b) b
fillId a = fill (Identity a)

fillIdF :: Functor f => f a -> Getting g (f (Identity a) -> b) b
fillIdF a = fill (idF a)
