{-# LANGUAGE DeriveGeneric, FlexibleContexts, MultiParamTypeClasses, OverloadedStrings #-}
module Backend.Conduit.Database
  ( ConduitDb(..)
  , QueryError(..)
  , conduitDb
  , maybeRow
  , openConduitDb
  , rowList
  , singleRow
  ) where

import           Control.Exception          (Exception)
import           Control.Monad.Error.Class  (MonadError, throwError)
import           Data.ByteString            (ByteString)
import           Data.Conduit               (ConduitT, (.|))
import qualified Data.Conduit               as Conduit
import qualified Data.Conduit.List          as Conduit
import           Database.Beam              (Database, DatabaseSettings, MonadIO, TableEntity, dbModification,
                                             defaultDbSettings, fieldNamed, liftIO, modifyTable,
                                             tableModification, withDbModification)
import           Database.Beam.Postgres     (Postgres)
import           Database.PostgreSQL.Simple (Connection, connectPostgreSQL)
import           GHC.Generics               (Generic)



import           Backend.Conduit.Database.Articles.Article    (ArticleT)
import qualified Backend.Conduit.Database.Articles.Article    as Article
import           Backend.Conduit.Database.Articles.ArticleTag (ArticleTagT)
import           Backend.Conduit.Database.Articles.Favorite   (FavoriteT)
import           Backend.Conduit.Database.Comments.Comment    (CommentT)
import qualified Backend.Conduit.Database.Comments.Comment    as Comment
import           Backend.Conduit.Database.Tags.Tag            (TagT)
import           Backend.Conduit.Database.Users.Follow        (FollowT)
import           Backend.Conduit.Database.Users.User          (UserT)


data ConduitDb f = ConduitDb
  { conduitArticleTags :: f (TableEntity ArticleTagT)
  , conduitArticles    :: f (TableEntity ArticleT)
  , conduitComments    :: f (TableEntity CommentT)
  , conduitFavorites   :: f (TableEntity FavoriteT)
  , conduitFollows     :: f (TableEntity FollowT)
  , conduitTags        :: f (TableEntity TagT)
  , conduitUsers       :: f (TableEntity UserT)
  } deriving (Generic)

instance Database Postgres ConduitDb

newtype QueryError = UnexpectedAmountOfRows Int
  deriving Show

instance Exception QueryError

conduitDb :: DatabaseSettings Postgres ConduitDb
conduitDb =
  defaultDbSettings `withDbModification`
  dbModification
    { conduitArticles =
        modifyTable id $
        tableModification
          { Article.createdAt = fieldNamed "created_at"
          , Article.updatedAt = fieldNamed "updated_at"
          }
    , conduitComments =
        modifyTable id $
        tableModification
          { Comment.createdAt = fieldNamed "created_at"
          , Comment.updatedAt = fieldNamed "updated_at"
          }
    }

openConduitDb :: MonadIO m => ByteString -> m Connection
openConduitDb = liftIO . connectPostgreSQL

maybeRow :: Monad m => ConduitT () a m () -> m (Maybe a)
maybeRow c = Conduit.runConduit (c .| Conduit.await)

singleRow :: (MonadError QueryError m) => ConduitT () a m () -> m a
singleRow c = maybe (throwError (UnexpectedAmountOfRows 0)) pure =<< maybeRow c

rowList :: Monad m => ConduitT () a m () -> m [a]
rowList c = Conduit.runConduit (c .| Conduit.consume)
