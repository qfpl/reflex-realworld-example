{-# LANGUAGE DataKinds, DeriveGeneric, FlexibleContexts, GeneralizedNewtypeDeriving, LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, RankNTypes, TemplateHaskell              #-}
module Backend.Conduit
  ( module Backend.Conduit
  , Claim
  ) where

import Control.Lens hiding (Context, (??))

import           Control.Error              ((??))
import           Control.Monad              (when)
import           Control.Monad.Except       (ExceptT (ExceptT), runExceptT, throwError, withExceptT)
import           Control.Monad.IO.Class     (MonadIO, liftIO)
import           Control.Monad.Reader       (ReaderT, runReaderT)
import           Crypto.JOSE.JWK            (JWK)
import           Data.Maybe                 (fromMaybe)
import           Data.Pool                  (Pool, createPool, withResource)
import qualified Data.Set                   as Set
import           Data.Text                  (Text, pack)
import           Data.Text.Encoding         (encodeUtf8)
import           Database.Beam              (primaryKey)
import           Database.PostgreSQL.Simple (Connection, close)
import           Servant                    ((:<|>) ((:<|>)), Context ((:.), EmptyContext),
                                             NoContent (NoContent), Server)
import           Servant.Auth.Server        (AuthResult (Authenticated, Indefinite), CookieSettings,
                                             JWTSettings, defaultCookieSettings, defaultJWTSettings)
import           Snap.Core                  (Snap)


import           Backend.Conduit.Claim                     (Claim (Claim), deriveToken)
import qualified Backend.Conduit.Database                  as Database
import qualified Backend.Conduit.Database.Articles         as DBArticles
import qualified Backend.Conduit.Database.Articles.Article as DBArticle
import qualified Backend.Conduit.Database.Comments         as DBComments
import qualified Backend.Conduit.Database.Tags             as DBTags
import qualified Backend.Conduit.Database.Users            as DBUsers
import qualified Backend.Conduit.Database.Users.User       as DBUser
import           Backend.Conduit.Errors
import           Common.Conduit.Api                        as Api
import qualified Common.Conduit.Api.Articles.Article       as ApiArticle
import qualified Common.Conduit.Api.Articles.Articles      as ApiArticles
import qualified Common.Conduit.Api.Articles.Comment       as ApiComment
import qualified Common.Conduit.Api.Articles.CreateComment as ApiCreateComment
import qualified Common.Conduit.Api.Profiles.Profile       as ApiProfile
import qualified Common.Conduit.Api.User.Account           as ApiAccount

data ConduitServerEnv = ConduitServerEnv
  { _dbPool      :: Pool Connection
  , _jwtSettings :: JWTSettings
  }
makeLenses ''ConduitServerEnv

type ConduitServerM   = ReaderT ConduitServerEnv Snap
type ConduitServerDbM = ConduitErrorsT (ReaderT Connection IO) --Concrete type for DB queries
type ConduitServerContext = '[CookieSettings, JWTSettings]

runConduitServerM :: ConduitServerEnv -> ConduitServerM a -> Snap a
runConduitServerM e = flip runReaderT e

mkContext :: JWTSettings -> Context ConduitServerContext
mkContext jwtS = defaultCookieSettings :. jwtS :. EmptyContext

mkEnv :: MonadIO m => Text -> JWK -> m ConduitServerEnv
mkEnv dbConnStr jwk = do
  p <- liftIO $ createPool (Database.openConduitDb (encodeUtf8 dbConnStr)) close 1 10 8
  pure $ ConduitServerEnv p (defaultJWTSettings jwk)

server :: Server (Api Claim) ConduitServerContext ConduitServerM
server = usersServer :<|> userServer :<|> articlesServer :<|> profileServer :<|> tagsServer

usersServer :: Server (UsersApi Claim) ConduitServerContext ConduitServerM
usersServer = loginServer :<|> registerServer
  where
    loginServer (Namespace creds) = runConduitErrorsT $ do
      user   <- runDatabase $ do
        credMay <- liftQuery $ DBUsers.findByCredentials creds
        credMay ?? forbidden
      userToAccount user

    registerServer (Namespace registrant) = runConduitErrorsT $ do
      user <- runDatabase $ do
        validReg <- withExceptT failedValidation $ DBUsers.validateRegistrant registrant
        liftQuery $ DBUsers.create validReg
      userToAccount user

userServer :: Server (UserApi Claim) ConduitServerContext ConduitServerM
userServer = currentUserServer :<|> updateUserServer
  where
    currentUserServer authRes = runConduitErrorsT $ do
      user <- runDatabase $ loadAuthorizedUser authRes
      userToAccount user

    updateUserServer authRes (Namespace update) = runConduitErrorsT $ do
      newUser <- runDatabase $ do
        currUser    <- loadAuthorizedUser authRes
        validUpdate <- withExceptT failedValidation $ DBUsers.validateUpdateUser currUser update
        liftQuery $ DBUsers.update (DBUser.unUserId (primaryKey currUser)) validUpdate
      userToAccount newUser

articlesServer :: Server (ArticlesApi Claim) ConduitServerContext ConduitServerM
articlesServer = listArticlesServer :<|> createArticleServer :<|> feedServer :<|> articleServer
  where
    listArticlesServer authRes limit offset tags authors favorited = runConduitErrorsT $ do
      runDatabase $ do
        currUserMay <- optionallyLoadAuthorizedUser authRes
        ApiArticles.fromList <$>
          (DBArticles.all
           (primaryKey <$> currUserMay)
           (fromMaybe 20 limit)
           (fromMaybe 0 offset)
           (Set.fromList authors)
           (Set.fromList tags)
           (Set.fromList favorited))

    feedServer authRes limit offset = runConduitErrorsT $ do
      runDatabase $ do
        currUser <- loadAuthorizedUser authRes
        ApiArticles.fromList <$>
          (DBArticles.feed
           (primaryKey currUser)
           (fromMaybe 20 limit)
           (fromMaybe 0 offset))

    createArticleServer authRes (Namespace attrCreate) = runConduitErrorsT $ do
      runDatabase $ do
        currUser   <- loadAuthorizedUser authRes
        validAttrs <- withExceptT failedValidation $ DBArticles.validateAttributesForInsert attrCreate
        liftQuery $ Namespace <$> DBArticles.create (primaryKey currUser) validAttrs

    articleServer authRes slug = getArticleServer :<|> commentsServer
      where
        getArticleServer = runConduitErrorsT $ do
          article <- runDatabase loadArticle
          pure $ Namespace article

        loadArticle = do
          currUserMay <- optionallyLoadAuthorizedUser authRes
          articleMay  <- liftQuery $ DBArticles.find (primaryKey <$> currUserMay) slug
          articleMay ?? notFound ("Article(" <> slug <> ")")

        commentsServer = listCommentsServer :<|> createCommentServer :<|> deleteCommentServer

        listCommentsServer = runConduitErrorsT $ do
          runDatabase $ do
            currUser    <- optionallyLoadAuthorizedUser authRes
            _           <- loadArticle
            Namespace <$> DBComments.forArticle (primaryKey <$> currUser) slug


        createCommentServer (Namespace cc) = runConduitErrorsT $ do
          runDatabase $ do
            currUser    <- loadAuthorizedUser authRes
            article     <- loadArticle
            liftQuery $ Namespace <$> DBComments.create
              (primaryKey currUser)
              (DBArticle.ArticleId (ApiArticle.id article))
              (ApiCreateComment.body cc)

        deleteCommentServer cId = runConduitErrorsT $ do
          runDatabase $ do
            currUser    <- loadAuthorizedUser authRes
            _           <- loadArticle
            commentMay  <- liftQuery $ DBComments.find (Just $ primaryKey currUser) cId
            comment     <- commentMay ?? notFound ("Comment("<> (pack $ show cId) <>")")
            when (ApiProfile.username (ApiComment.author comment) /= DBUser.username currUser) $
              throwError forbidden
            liftQuery $ DBComments.destroy cId
            pure NoContent

profileServer :: Server (ProfilesApi Claim) ConduitServerContext ConduitServerM
profileServer = profileGetServer
  where
    profileGetServer authRes username = runConduitErrorsT $ do
      runDatabase $ do
        currUserMay <- optionallyLoadAuthorizedUser authRes
        profileMay  <- liftQuery $ DBUsers.findProfile (primaryKey <$> currUserMay) username
        Namespace <$> (profileMay ?? (notFound ("Profile(" <> username <> ")")))

tagsServer :: Server (TagsApi Claim) ConduitServerContext ConduitServerM
tagsServer = tagsAllServer
  where
    tagsAllServer = runConduitErrorsT . runDatabase . liftQuery $ Namespace . fmap DBTags.name <$> DBTags.query

-- Helper Functions TODO Move to Internal Module -------------------------------------------------------------

userToAccount :: DBUser.User -> ConduitErrorsT ConduitServerM (Namespace "user" (ApiAccount.Account))
userToAccount user = do
  jwtS   <- view jwtSettings
  token  <- withExceptT (internalServerErrorShow "Couldn't make JWT") $ deriveToken jwtS user
  pure . Namespace $ Account
    { ApiAccount.email    = DBUser.email user
    , ApiAccount.token    = Api.Token token
    , ApiAccount.bio      = DBUser.bio user
    , ApiAccount.username = DBUser.username user
    , ApiAccount.image    = DBUser.image user
    }

-- We put ExceptT on top so it is easier to use (!?) and friends
runDatabase :: ConduitServerDbM a -> ConduitErrorsT ConduitServerM a
runDatabase m = do
  p <- view dbPool
  -- TODO: There should probably be a transaction here ;)
  ExceptT . liftIO . withResource p $ runReaderT (runExceptT m)

liftQuery :: ExceptT Database.QueryError (ReaderT Connection IO) a -> ConduitErrorsT (ReaderT Connection IO) a
liftQuery = withExceptT (internalServerErrorShow "QueryError")

loadAuthorizedUser :: AuthResult Claim -> ConduitServerDbM DBUser.User
loadAuthorizedUser authResult =
  case authResult of
    Authenticated (Claim identifier) -> do
      uMay <- liftQuery (DBUsers.find identifier)
      uMay ?? forbidden
    _ -> throwError forbidden

optionallyLoadAuthorizedUser :: AuthResult Claim -> ConduitServerDbM (Maybe DBUser.User)
optionallyLoadAuthorizedUser = \case
  Indefinite -> pure Nothing
  authResult -> Just <$> loadAuthorizedUser authResult
