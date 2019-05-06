{-# LANGUAGE EmptyCase, FlexibleContexts, FlexibleInstances, GADTs, KindSignatures, LambdaCase    #-}
{-# LANGUAGE MultiParamTypeClasses, OverloadedStrings, PolyKinds, RankNTypes, ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell, TypeFamilies                                                        #-}

module Common.Route where

import Control.Lens  hiding (bimap)
import Obelisk.Route
import Prelude       hiding (id, (.))

import           Control.Categorical.Bifunctor (bimap)
import           Control.Category              (id, (.))
import           Control.Monad.IO.Class        (MonadIO, liftIO)
import           Data.Functor.Identity         (Identity)
import           Data.Functor.Sum              (Sum (InL, InR))
import           Data.Text                     (Text)
import qualified Data.Text                     as T
import qualified Obelisk.ExecutableConfig      as ObConfig
import           Obelisk.Route.TH              (deriveRouteComponent)


newtype DocumentSlug = DocumentSlug { unDocumentSlug :: Text } deriving (Eq, Ord, Show)
makeWrapped ''DocumentSlug
newtype Username = Username { unUsername :: Text } deriving (Eq, Ord, Show)
makeWrapped ''Username

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  BackendRoute_Api :: BackendRoute PageName

data FrontendRoute :: * -> * where
  FrontendRoute_Home :: FrontendRoute ()
  FrontendRoute_Login :: FrontendRoute ()
  FrontendRoute_Register :: FrontendRoute ()
  FrontendRoute_Settings :: FrontendRoute ()
  FrontendRoute_Editor :: FrontendRoute (Maybe DocumentSlug)
  FrontendRoute_Article :: FrontendRoute DocumentSlug
  FrontendRoute_Profile :: FrontendRoute (Username, Maybe (R ProfileRoute))
  -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.

data ProfileRoute :: * -> * where
  ProfileRoute_Favourites :: ProfileRoute ()

backendRouteEncoder
  :: Encoder (Either Text) Identity (R (Sum BackendRoute (ObeliskRoute FrontendRoute))) PageName
backendRouteEncoder = handleEncoder (const (InL BackendRoute_Missing :/ ())) $
  pathComponentEncoder $ \case
    InL backendRoute -> case backendRoute of
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
      BackendRoute_Api     -> PathSegment "api" $ id
    InR obeliskRoute -> obeliskRouteSegment obeliskRoute $ \case
      FrontendRoute_Home -> PathEnd $ unitEncoder mempty
      FrontendRoute_Login -> PathSegment "login" $ unitEncoder mempty
      FrontendRoute_Register -> PathSegment "register" $ unitEncoder mempty
      FrontendRoute_Settings -> PathSegment "settings" $ unitEncoder mempty
      FrontendRoute_Editor -> PathSegment "editor" $ maybeEncoder (unitEncoder mempty) (singlePathSegmentEncoder . unwrappedEncoder)
      FrontendRoute_Article -> PathSegment "article" $ singlePathSegmentEncoder . unwrappedEncoder
      FrontendRoute_Profile -> PathSegment "profile" $
        let profileRouteEncoder = pathComponentEncoder $ \case
              ProfileRoute_Favourites -> PathSegment "favourites" $ unitEncoder mempty
        in ( pathSegmentEncoder . bimap unwrappedEncoder (maybeEncoder (unitEncoder mempty) profileRouteEncoder ) )

getAppRoute :: MonadIO m => m Text
getAppRoute = do
    mroute <- liftIO $ ObConfig.get "config/common/route"
    case mroute of
      Nothing -> fail "Error getAppRoute: config/common/route not defined"
      Just r  -> return $ T.dropWhileEnd (== '/') $ T.strip r

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  , ''ProfileRoute
  ]
