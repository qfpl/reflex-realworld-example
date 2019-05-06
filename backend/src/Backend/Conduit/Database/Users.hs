{-# LANGUAGE FlexibleContexts, OverloadedStrings #-}
module Backend.Conduit.Database.Users
  ( ProfileResult
  , ProfileRow
  , validateRegistrant
  , validateUpdateUser
  , create
  , find
  , findByCredentials
  , findProfile
  , follow
  , selectProfiles
  , unfollow
  , update
  ) where

import           Control.Lens                    (view, (^.), _1, _2)
import           Control.Monad.Error.Class       (MonadError, throwError)
import           Control.Monad.IO.Class          (MonadIO, liftIO)
import           Control.Monad.Reader.Class      (MonadReader, ask)
import           Control.Monad.Trans.Control     (MonadBaseControl)
import           Crypto.Scrypt                   (EncryptedPass (EncryptedPass, getEncryptedPass),
                                                  Pass (Pass), encryptPassIO', verifyPass')
import           Data.Functor                    (void)
import           Data.Functor.Compose            (Compose (Compose, getCompose))
import qualified Data.Map                        as Map
import           Data.Maybe                      (catMaybes, fromMaybe)
import           Data.Text                       (Text)
import qualified Data.Text                       as Text
import           Data.Text.Encoding              (decodeUtf8, encodeUtf8)
import           Data.Validation                 (Validation (Failure, Success), validation)
import           Database.Beam.Postgres.Extended (HasSqlEqualityCheck, Nullable, PgExpressionSyntax,
                                                  PgInsertReturning, PgQExpr, PgSelectSyntax,
                                                  PgUpdateReturning, Q, aggregate_, all_, default_, delete,
                                                  exists_, group_, guard_, insertExpressions, insertReturning,
                                                  just_, leftJoin_, onConflictDefault, pgBoolOr, primaryKey,
                                                  references_, runDelete, runInsertReturning, runSelect,
                                                  runUpdateReturning, select, updateReturning, val_, (&&.),
                                                  (<-.), (==.))
import           Database.PostgreSQL.Simple      (Connection)

import           Backend.Conduit.Database              (ConduitDb (conduitFollows, conduitUsers), QueryError,
                                                        conduitDb, maybeRow, singleRow)
import           Backend.Conduit.Database.Users.Follow (Follow, FollowT (Follow))
import qualified Backend.Conduit.Database.Users.Follow as Follow
import           Backend.Conduit.Database.Users.User   (PrimaryKey (UserId), User, UserId, UserT (User))
import qualified Backend.Conduit.Database.Users.User   as User
import           Backend.Conduit.Validation            (ValidationErrors, requiredText)
import           Common.Conduit.Api.Profiles.Profile   (Profile (Profile))
import           Common.Conduit.Api.User.Update        (UpdateUser (UpdateUser))
import qualified Common.Conduit.Api.User.Update        as UpdateUser
import           Common.Conduit.Api.Users.Credentials  (Credentials)
import qualified Common.Conduit.Api.Users.Credentials  as Credentials
import           Common.Conduit.Api.Users.Registrant   (Registrant (Registrant))
import qualified Common.Conduit.Api.Users.Registrant   as Registrant
insertUser
  :: Registrant -> PgInsertReturning User
insertUser reg
  = insertReturning
    (conduitUsers conduitDb)
    (insertExpressions
      [ User
          { User.id = default_
          , User.password = val_ (Registrant.password reg)
          , User.email = val_ (Registrant.email reg)
          , User.bio = val_ ""
          , User.username = val_ (Registrant.username reg)
          , User.image = default_
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
  => Registrant
  -> m User
create reg = do
  conn <- ask
  runInsertReturning conn (insertUser reg) singleRow

updateUser
  :: UserId -> UpdateUser -> PgUpdateReturning User
updateUser userId updateU
  = updateReturning
    (conduitUsers conduitDb)
    (\user -> catMaybes
      [ (User.password user <-.) . val_ <$> UpdateUser.password updateU
      , (User.email user <-.) . val_ <$> UpdateUser.email updateU
      , (User.username user <-.) . val_ <$> UpdateUser.username updateU
      , (User.bio user <-.) . val_ <$> UpdateUser.bio updateU
      , (User.image user <-.) . val_ <$> pure (UpdateUser.image updateU)
      ]
    )
    ((val_ userId ==.) . primaryKey)
    id

update
  :: ( MonadReader Connection m
     , MonadError QueryError m
     , MonadIO m
     , MonadBaseControl IO m
     )
  => Int
  -> UpdateUser
  -> m User
update userId updateU = do
  conn <- ask
  runUpdateReturning
    conn
    (updateUser (UserId userId) updateU)
    singleRow

follow
  :: ( MonadReader Connection m
     , MonadIO m
     , MonadBaseControl IO m
     , MonadError QueryError m
     )
  => UserId
  -> UserId
  -> m Follow
follow followerId followeeId = do
  conn <- ask
  runInsertReturning
    conn
    (insertReturning
      (conduitFollows conduitDb)
      (insertExpressions
        [ Follow
            { Follow.follower = val_ followerId
            , Follow.followee = val_ followeeId
            }
        ]
      )
      onConflictDefault
      (Just id)
    )
    singleRow

unfollow
  :: (MonadReader Connection m, MonadIO m)
  => UserId
  -> UserId
  -> m ()
unfollow followerId followeeId = do
  conn <- ask
  void
    $ runDelete conn
    $ delete (conduitFollows conduitDb)
    $ \(Follow follower followee) ->
        follower ==. val_ followerId &&. followee ==. val_ followeeId

encryptPassword :: Text -> IO Text
encryptPassword password =
  decodeUtf8 . getEncryptedPass <$> encryptPassIO' (Pass (encodeUtf8 password))

makePassword :: MonadIO m => Text -> Compose m (Validation ValidationErrors) Text
makePassword value
  | Text.length value < 8 = Compose $ pure $ Failure
    (Map.singleton "password" ["Must be longer than 8 chars"])
  | otherwise = Compose $ Success <$> liftIO (encryptPassword value)

usernameExists
  :: (MonadIO m, MonadReader Connection m, MonadBaseControl IO m)
  => Text
  -> m Bool
usernameExists username = do
  conn <- ask
  fromMaybe False <$> runSelect conn (select query) maybeRow
  where
    query = pure $ exists_ $ selectUserBy User.username (val_ username)

emailExists
  :: (MonadIO m, MonadReader Connection m, MonadBaseControl IO m)
  => Text
  -> m Bool
emailExists email = do
  conn <- ask
  fromMaybe False <$> runSelect conn (select query) maybeRow
  where
    query = pure $ exists_ $ selectUserBy User.email (val_ email)

uniqueEmail
  :: (MonadIO m, MonadReader Connection m, MonadBaseControl IO m)
  => Text
  -> Compose m (Validation ValidationErrors) Text
uniqueEmail value =
  Compose $ do
    exists <- emailExists value
    pure $
      if exists
        then Failure (Map.singleton "email" ["Taken"])
        else Success value

makeEmail
  :: (MonadIO m, MonadReader Connection m, MonadBaseControl IO m)
  => Text
  -> Compose m (Validation ValidationErrors) Text
makeEmail email =
  requiredText "email" email *> uniqueEmail email

uniqueUsername
  :: (MonadIO m, MonadReader Connection m, MonadBaseControl IO m)
  => Text
  -> Compose m (Validation ValidationErrors) Text
uniqueUsername value =
  Compose $ do
    exists <- usernameExists value
    pure $
      if exists
        then Failure (Map.singleton "username" ["Taken"])
        else Success value

makeUsername
  :: (MonadIO m, MonadReader Connection m, MonadBaseControl IO m)
  => Text
  -> Compose m (Validation ValidationErrors) Text
makeUsername username =
  requiredText "username" username *> uniqueUsername username

validateRegistrant
  :: ( MonadIO m
     , MonadReader Connection m
     , MonadBaseControl IO m
     , MonadError ValidationErrors m
     )
  => Registrant
  -> m Registrant
validateRegistrant reg =
  (validation throwError pure =<<) . getCompose $
  Registrant
    <$> makeUsername (Registrant.username reg)
    <*> makeEmail (Registrant.email reg)
    <*> makePassword (Registrant.password reg)

makeUpdateEmail
  :: (MonadIO m, MonadReader Connection m, MonadBaseControl IO m)
  => User
  -> Text
  -> Compose m (Validation ValidationErrors) Text
makeUpdateEmail current value
  | value == User.email current = pure value
  | otherwise = makeEmail value

makeUpdateUsername
  :: (MonadIO m, MonadReader Connection m, MonadBaseControl IO m)
  => User
  -> Text
  -> Compose m (Validation ValidationErrors) Text
makeUpdateUsername current value
  | value == User.username current = pure value
  | otherwise = makeUsername value

validateUpdateUser
  :: ( MonadIO m
     , MonadReader Connection m
     , MonadBaseControl IO m
     , MonadError ValidationErrors m
     )
  => User
  -> UpdateUser
  -> m UpdateUser
validateUpdateUser current uu =
  (validation throwError pure =<<) . getCompose $
  UpdateUser
    <$> traverse makePassword (UpdateUser.password uu)
    <*> traverse (makeUpdateEmail current) (UpdateUser.email uu)
    <*> traverse (makeUpdateUsername current) (UpdateUser.username uu)
    <*> pure (UpdateUser.bio uu)
    <*> pure (UpdateUser.image uu)

selectUserBy
  :: HasSqlEqualityCheck PgExpressionSyntax a
  => (UserT (PgQExpr s) -> PgQExpr s a)
  -> PgQExpr s a
  -> Q PgSelectSyntax ConduitDb s (UserT (PgQExpr s))
selectUserBy f a = do
  user <- all_ (conduitUsers conduitDb)
  guard_ (f user ==. a)
  pure user

find
  :: (MonadReader Connection m, MonadIO m, MonadBaseControl IO m)
  => Int
  -> m (Maybe User)
find userId = do
  conn <- ask
  runSelect conn (select (selectUserBy User.id (val_ userId))) maybeRow

findByEmail
  :: (MonadReader Connection m, MonadIO m, MonadBaseControl IO m)
  => Text
  -> m (Maybe User)
findByEmail email = do
  conn <- ask
  runSelect conn (select (selectUserBy User.email (val_ email))) maybeRow

encryptedPassMatches :: Text -> Text -> Bool
encryptedPassMatches a b =
  verifyPass' (Pass (encodeUtf8 a)) (EncryptedPass (encodeUtf8 b))

findByCredentials
  :: (MonadReader Connection m, MonadIO m, MonadBaseControl IO m)
  => Credentials
  -> m (Maybe User)
findByCredentials credentials = do
  found <- findByEmail (Credentials.email credentials)
  if maybe False (encryptedPassMatches (Credentials.password credentials) . User.password) found
    then pure found
    else pure Nothing

follows
  :: UserT (PgQExpr s)
  -> Q PgSelectSyntax ConduitDb s (FollowT (Nullable (PgQExpr s)))
follows author =
  leftJoin_
    (all_ (conduitFollows conduitDb))
    ((`references_` author) . Follow.followee)

type ProfileRow s =
  ( UserT (PgQExpr s)
  , PgQExpr s (Maybe Bool)
  )

type ProfileResult =
  ( User
  , Maybe Bool
  )

selectProfiles
  :: Maybe UserId
  -> Q PgSelectSyntax ConduitDb s (ProfileRow s)
selectProfiles currentUserId =
  aggregate_ (\(user, following) -> (group_ user, pgBoolOr following)) $ do
    user <- all_ (conduitUsers conduitDb)
    follow' <- follows user
    pure
      ( user
      , maybe
          (val_ False)
          ((Follow.follower follow' ==.) . just_ . val_)
          currentUserId)

selectProfile
  :: Maybe UserId
  -> Text
  -> Q PgSelectSyntax ConduitDb s (ProfileRow s)
selectProfile currentUserId username = do
  profile <- selectProfiles currentUserId
  guard_ $ User.username (profile ^. _1) ==. val_ username
  pure profile

toProfile :: ProfileResult -> Profile
toProfile =
  Profile
    <$> (User.id . view _1)
    <*> (User.username . view _1)
    <*> (User.bio . view _1)
    <*> (User.image . view _1)
    <*> (fromMaybe False . view _2)

findProfile
  :: (MonadReader Connection m, MonadIO m, MonadBaseControl IO m)
  => Maybe UserId
  -> Text
  -> m (Maybe Profile)
findProfile currentUserId username = do
  conn <- ask
  fmap toProfile
    <$> runSelect conn (select (selectProfile currentUserId username)) maybeRow
