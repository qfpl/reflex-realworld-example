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
{-# LANGUAGE ScopedTypeVariables   #-}

module Common.Route where

import           Control.Lens
import           Obelisk.Route

import           Control.Category      ((.))
import           Control.Monad.Except  (MonadError)
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
    InR obeliskRoute -> obeliskRouteSegment obeliskRoute $ \case
      -- The encoder given to PathEnd determines how to parse query parameters,
      -- in this example, we have none, so we insist on it.
      FrontendRoute_Home -> PathEnd $ unitEncoder mempty
      FrontendRoute_Login -> PathSegment "login" $ unitEncoder mempty
      FrontendRoute_Register -> PathSegment "register" $ unitEncoder mempty
      FrontendRoute_Settings -> PathSegment "settings" $ unitEncoder mempty
      FrontendRoute_Editor -> PathSegment "editor" $ maybeEncoder (unitEncoder mempty) (singlePathSegmentEncoder . unwrappedEncoder)
      FrontendRoute_Article -> PathSegment "article" $ singlePathSegmentEncoder . unwrappedEncoder
      -- Lets ignore the favourites route for now
      FrontendRoute_Profile -> PathSegment "profile" $
        pathSegmentConsEncoder unwrappedEncoder $ maybeEncoder (unitEncoder mempty) $ pathComponentEncoder $ \case
          ProfileRoute_Favourites -> PathSegment "favourites" $ unitEncoder mempty
 --     FrontendRoute_Profile -> PathSegment "profile" $ unicorn

unicorn
  :: (MonadError Text parse, check ~ parse)
  => Encoder check parse (Username, Maybe (R ProfileRoute)) PageName
unicorn = segmentParam unwrappedEncoder profileRMayEnc -- This may be nonsensical. Follow reasoning below
  where
    -- I know how to do these two things separately! But it doesn't feel like this is the way to go
    -- because both encoders would encode into query params and could clash horribly
    usernameEnc :: (Applicative check, MonadError Text parse) => Encoder check parse Username PageName
    usernameEnc = singlePathSegmentEncoder . unwrappedEncoder
    profileRMayEnc :: (MonadError Text check, check ~ parse) => Encoder check parse (Maybe (R ProfileRoute)) PageName
    profileRMayEnc = maybeEncoder (unitEncoder mempty) $ pathComponentEncoder $ \case
      ProfileRoute_Favourites -> PathEnd $ unitEncoder mempty
    -- It feels like you want something that can peel a pathPart off and handle the rest as a sub
    -- like the maybeEncoder, but it's a 1 or more encoder that doesn't lose the params.
    segmentParam
      :: MonadError Text parse
      => Encoder check parse a Text
      -> Encoder check parse b PageName
      -> Encoder check parse (a,b) PageName
    segmentParam = undefined
    -- But I don't know whether this makes sense or could be implemented lawfully
    -- This kinda looks like chainEncoder, but chainEncoder doesn't feel like it

    -- I may just missing the implication of an important instance on Encoder
    -- * Category (Encoder check parse)
    -- * Associative (Encoder check parse) (,)
    -- * Monoidal (Encoder check parse) (,)
    -- * Cat.Functor f (Encoder check parse) (Encoder check parse)
    -- * PFunctor (,) (Encoder check parse) (Encoder check parse)
    -- * QFunctor (,) (Encoder check parse) (Encoder check parse)
    -- * Bifunctor (,) (Encoder check parse) (Encoder check parse)

concat <$> mapM deriveRouteComponent
  [ ''BackendRoute
  , ''FrontendRoute
  , ''ProfileRoute
  ]
