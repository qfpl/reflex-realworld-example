{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Common.Route where

{- -- You will probably want these imports for composing Encoders.
import Prelude hiding (id, (.))
import Control.Category
-}

import Data.Text (Text)
import Data.Functor.Identity
import Data.Functor.Sum

import Obelisk.Route
import Obelisk.Route.TH

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
  FrontendRoute_Create :: FrontendRoute ()
  FrontendRoute_Edit :: FrontendRoute ()
  FrontendRoute_Article :: FrontendRoute ()
  FrontendRoute_Profile :: FrontendRoute ()
  FrontendRoute_ProfileFavourites :: FrontendRoute ()
  -- This type is used to define frontend routes, i.e. ones for which the backend will serve the frontend.

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
      FrontendRoute_Create -> PathSegment "editor" $ unitEncoder mempty
      FrontendRoute_Edit -> PathSegment "editoredit" $ unitEncoder mempty
      FrontendRoute_Article -> PathSegment "article" $ unitEncoder mempty
      FrontendRoute_Profile -> PathSegment "profile" $ unitEncoder mempty
      FrontendRoute_ProfileFavourites -> PathSegment "profilefavs" $ unitEncoder mempty


concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  ]
