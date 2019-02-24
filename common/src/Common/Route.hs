{-# LANGUAGE EmptyCase             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

module Common.Route where

import           Control.Lens
import           Obelisk.Route

import           Control.Category      ((.))
import           Data.Functor.Identity (Identity)
import           Data.Functor.Sum      (Sum (InL, InR))
import           Data.Text             (Text)
import           Prelude               hiding (id, (.))

import           Obelisk.Route.TH      (deriveRouteComponent)

newtype DocumentSlug = DocumentSlug { unDocumentSlug :: Text } deriving (Eq, Ord, Show)
makeWrapped ''DocumentSlug
newtype Username = Username { unUsername :: Text } deriving (Eq, Ord, Show)
makeWrapped ''Username

data BackendRoute :: * -> * where
  -- | Used to handle unparseable routes.
  BackendRoute_Missing :: BackendRoute ()
  -- You can define any routes that will be handled specially by the backend here.
  -- i.e. These do not serve the frontend, but do something different, such as serving static files.

data FrontendRoute :: * -> * where
  FrontendRoute_Home :: FrontendRoute ()
  FrontendRoute_Login :: FrontendRoute ()
  FrontendRoute_Register :: FrontendRoute ()
  FrontendRoute_Settings :: FrontendRoute ()
  FrontendRoute_Editor :: FrontendRoute (Maybe DocumentSlug)
  FrontendRoute_Article :: FrontendRoute DocumentSlug
  -- We actually want a username and an optional subroute, but can't figure out
  -- how to do that just yet.
  -- FrontendRoute_Profile :: FrontendRoute (Username, Maybe (R ProfileRoute))
  FrontendRoute_Profile :: FrontendRoute Username
  -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.

data ProfileRoute :: * -> * where
  ProfileRoute_Favourites :: ProfileRoute ()

backendRouteEncoder
  :: Encoder (Either Text) Identity (R (Sum BackendRoute (ObeliskRoute FrontendRoute))) PageName
backendRouteEncoder = handleEncoder (const (InL BackendRoute_Missing :/ ())) $
  pathComponentEncoder $ \case
    InL backendRoute -> case backendRoute of
      BackendRoute_Missing -> PathSegment "missing" $ unitEncoder mempty
    InR obeliskRoute -> obeliskRouteSegment obeliskRoute $ \case
      -- The encoder given to PathEnd determines how to parse query parameters,
      -- in this example, we have none, so we insist on it.
      FrontendRoute_Home -> PathEnd $ unitEncoder mempty
      FrontendRoute_Login -> PathSegment "login" $ unitEncoder mempty
      FrontendRoute_Register -> PathSegment "register" $ unitEncoder mempty
      FrontendRoute_Settings -> PathSegment "settings" $ unitEncoder mempty
      FrontendRoute_Editor -> PathSegment "editor" $ maybeEncoder (unitEncoder mempty) (singlePathSegmentEncoder . unwrappedEncoder)
      FrontendRoute_Article -> PathSegment "article" $ singlePathSegmentEncoder . unwrappedEncoder
      FrontendRoute_Profile -> PathSegment "profile" $ singlePathSegmentEncoder . unwrappedEncoder
 --     FrontendRoute_Profile -> PathSegment "profile" $ unicorn

unicorn :: Encoder check parse (Username, Maybe (R ProfileRoute)) PageName
unicorn = undefined
  -- Notes:
  -- * chainEncoder almost looks right, but doesn't seem it
  -- * probably just missing the implication of an important instance on Encoder
  -- ** Category (Encoder check parse)
  -- ** Associative (Encoder check parse) (,)
  -- ** Monoidal (Encoder check parse) (,)
  -- ** Cat.Functor f (Encoder check parse) (Encoder check parse)
  -- ** PFunctor (,) (Encoder check parse) (Encoder check parse)
  -- ** QFunctor (,) (Encoder check parse) (Encoder check parse)
  -- ** Bifunctor (,) (Encoder check parse) (Encoder check parse)

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  , ''ProfileRoute
  ]
