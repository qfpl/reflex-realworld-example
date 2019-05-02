module Backend.Conduit.Tags.Database
  ( create
  , query
  , TagT(..)
  , Tag
  , TagId
  ) where

import Control.Monad.Trans.Control (MonadBaseControl)
import Database.Beam.Postgres.Extended
  ( all_
  , conflictingFields
  , insertExpressions
  , insertReturning
  , onConflict
  , onConflictUpdateInstead
  , runInsertReturning
  , runSelect
  , select
  , val_
  )
import Database.PostgreSQL.Simple (Connection)
import Backend.Conduit.Database (ConduitDb(..), conduitDb, rowList)
import Backend.Conduit.Tags.Database.Tag (Tag, TagId, TagT(Tag))
import qualified Backend.Conduit.Tags.Database.Tag as Tag

query
  :: (MonadReader Connection m, MonadIO m, MonadBaseControl IO m)
  => m [Tag]
query = do
  conn <- ask
  runSelect conn (select (all_ (conduitTags conduitDb))) rowList

create
  :: (MonadReader Connection m, MonadIO m, MonadBaseControl IO m)
  => Set Text
  -> m [Tag]
create names = do
  conn <- ask
  runInsertReturning
    conn
    (insertReturning
       (conduitTags conduitDb)
       (insertExpressions (map (Tag . val_) (toList names)))
       (onConflict (conflictingFields Tag.name) (onConflictUpdateInstead Tag.name))
       (Just id))
    rowList
