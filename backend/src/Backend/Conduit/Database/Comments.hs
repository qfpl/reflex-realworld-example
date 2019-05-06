{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Backend.Conduit.Database.Comments
  ( create
  , destroy
  , find
  , forArticle
  ) where

import Control.Lens                    (view, (^.), _1, _2)
import Control.Monad.Error.Class       (MonadError)
import Control.Monad.IO.Class          (MonadIO, liftIO)
import Control.Monad.Reader.Class      (MonadReader, ask)
import Control.Monad.Trans.Control     (MonadBaseControl)
import Data.Functor                    (void)
import Data.Maybe                      (fromMaybe)
import Data.Text                       (Text)
import Data.Time                       (UTCTime, getCurrentTime)
import Database.Beam.Postgres.Extended (PgInsertReturning, PgQExpr, PgSelectSyntax, Q, all_, default_, delete,
                                        guard_, insertExpressions, insertReturning, onConflictDefault,
                                        primaryKey, runDelete, runInsertReturning, runSelect, select, val_,
                                        (==.))
import Database.PostgreSQL.Simple      (Connection)

import           Backend.Conduit.Database                  (ConduitDb (conduitArticles, conduitComments),
                                                            QueryError, conduitDb, maybeRow, rowList,
                                                            singleRow)
import           Backend.Conduit.Database.Articles.Article (ArticleId)
import qualified Backend.Conduit.Database.Articles.Article as PersistedArticle
import           Backend.Conduit.Database.Comments.Comment (PrimaryKey (CommentId))
import qualified Backend.Conduit.Database.Comments.Comment as Persisted
import           Backend.Conduit.Database.Users            (ProfileResult, ProfileRow, selectProfiles)
import           Backend.Conduit.Database.Users.User       (UserId)
import qualified Backend.Conduit.Database.Users.User       as User
import           Common.Conduit.Api.Articles.Comment       (Comment (Comment))
import           Common.Conduit.Api.Profiles.Profile       (Profile (Profile))

insertComment
  :: UserId -> ArticleId -> Text -> UTCTime -> PgInsertReturning Persisted.Comment
insertComment authorId articleId body currentTime
  = insertReturning
    (conduitComments conduitDb)
    (insertExpressions
      [ Persisted.Comment
           { Persisted.id = default_
           , Persisted.body = val_ body
           , Persisted.article = val_ articleId
           , Persisted.author = val_ authorId
           , Persisted.createdAt = val_ currentTime
           , Persisted.updatedAt = val_ currentTime
           }
      ]
    )
    onConflictDefault
    (Just id)

create
  :: ( MonadReader Connection m
     , MonadError QueryError m
     , MonadIO m
     , MonadBaseControl IO m
     )
  => UserId
  -> ArticleId
  -> Text
  -> m Comment
create authorId articleId body = do
  conn <- ask
  currentTime <- liftIO getCurrentTime
  inserted <-
    runInsertReturning
      conn
      (insertComment authorId articleId body currentTime)
      singleRow
  unsafeFind (Just authorId) (Persisted.id inserted)

destroy :: (MonadIO m, MonadReader Connection m) => Int -> m ()
destroy comment = do
  conn <- ask
  void $ runDelete conn $ delete (conduitComments conduitDb) $ \candidate ->
    primaryKey candidate ==. val_ (CommentId comment)

type CommentRow s =
  ( Persisted.CommentT (PgQExpr s)
  , ProfileRow s
  )

type CommentResult =
  ( Persisted.Comment
  , ProfileResult
  )

toComment :: CommentResult -> Comment
toComment =
  Comment
    <$> (Persisted.id . view _1)
    <*> (Persisted.createdAt . view _1)
    <*> (Persisted.updatedAt . view _1)
    <*> (Persisted.body . view _1)
    <*> (Profile
          <$> (User.id . view (_2 . _1))
          <*> (User.username . view (_2 . _1))
          <*> (User.bio . view (_2 . _1))
          <*> (User.image . view (_2 . _1))
          <*> (fromMaybe False . view (_2 . _2))
          )

selectComments
  :: Maybe UserId
  -> Q PgSelectSyntax ConduitDb s (CommentRow s)
selectComments currentUserId = do
  comment <- all_ (conduitComments conduitDb)
  profile <- selectProfiles currentUserId
  guard_ (Persisted.author comment ==. primaryKey (profile ^. _1))
  pure (comment, profile)

selectComment
  :: Maybe UserId
  -> Int
  -> Q PgSelectSyntax ConduitDb s (CommentRow s)
selectComment currentUserId commentId = do
  commentRow <- selectComments currentUserId
  guard_ (Persisted.id (commentRow ^. _1) ==. val_ commentId)
  pure commentRow

find
  :: (MonadReader Connection m, MonadIO m, MonadBaseControl IO m)
  => Maybe UserId
  -> Int
  -> m (Maybe Comment)
find currentUserId commentId = do
  conn <- ask
  fmap toComment <$> runSelect conn (select (selectComment currentUserId commentId)) maybeRow

unsafeFind
  :: ( MonadReader Connection m
     , MonadIO m
     , MonadBaseControl IO m
     , MonadError QueryError m
     )
  => Maybe UserId
  -> Int
  -> m Comment
unsafeFind currentUserId commentId = do
  conn <- ask
  toComment <$> runSelect conn (select (selectComment currentUserId commentId)) singleRow

selectCommentsForArticle
  :: Maybe UserId
  -> Text
  -> Q PgSelectSyntax ConduitDb s (CommentRow s)
selectCommentsForArticle currentUserId slug = do
  commentRow <- selectComments currentUserId
  article <- all_ (conduitArticles conduitDb)
  guard_ (PersistedArticle.slug article ==. val_ slug)
  guard_ (Persisted.article (commentRow ^. _1) ==. primaryKey article)
  pure commentRow

forArticle
  :: (MonadReader Connection m, MonadIO m, MonadBaseControl IO m)
  => Maybe UserId
  -> Text
  -> m [Comment]
forArticle currentUserId slug = do
  conn <- ask
  fmap toComment <$> runSelect conn (select (selectCommentsForArticle currentUserId slug)) rowList
