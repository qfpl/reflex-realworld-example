{-# LANGUAGE DeriveGeneric, FlexibleInstances, GADTs, GeneralizedNewtypeDeriving, MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, StandaloneDeriving, TemplateHaskell, TypeFamilies  #-}
{-# LANGUAGE UndecidableInstances                                                                       #-}
{-# OPTIONS_GHC -fno-warn-orphans       #-}
module Frontend.LocalStorageKey where

import Data.Aeson            (FromJSON (..), ToJSON (..))
import Data.Dependent.Map    (Some (This))
import Data.Functor.Identity (Identity (Identity))
import Data.GADT.Aeson       (FromJSONTag (..), GKey (..), ToJSONTag (..))
import Data.GADT.Compare.TH  (deriveGCompare, deriveGEq)
import Data.GADT.Show.TH     (deriveGShow)

import Control.Monad.Trans      (lift)
import Obelisk.Route.Frontend   (RouteToUrl (..), Routed (..), RoutedT, SetRoute (..))
import Reflex                   (EventWriterT)
import Reflex.Dom.Storage.Base  (StorageT (..))
import Reflex.Dom.Storage.Class (HasStorage (..))

import Common.Conduit.Api.User.Account (Token)

data LocalStorageTag a where
  LocalStorageJWT :: LocalStorageTag Token

deriveGEq ''LocalStorageTag
deriveGCompare ''LocalStorageTag
deriveGShow ''LocalStorageTag

instance GKey LocalStorageTag where
  toKey (This LocalStorageJWT) = "conduit_jwt"

  fromKey t =
    case t of
      "conduit_jwt" -> Just (This LocalStorageJWT)
      _             -> Nothing

  keys _ = [This LocalStorageJWT]

instance ToJSONTag LocalStorageTag Identity where
  toJSONTagged LocalStorageJWT (Identity x) = toJSON x

instance FromJSONTag LocalStorageTag Identity where
  parseJSONTagged LocalStorageJWT x = Identity <$> parseJSON x

instance (Monad m, SetRoute t r m) => SetRoute t r (StorageT t k m) where
  setRoute = lift . setRoute
  modifyRoute = lift . modifyRoute

instance (Monad m, RouteToUrl r m) => RouteToUrl r (StorageT t k m) where
  askRouteToUrl = lift askRouteToUrl

instance (Monad m, Routed t r m) => Routed t r (StorageT t k m) where
  askRoute = lift askRoute

instance HasStorage t k m => HasStorage t k (RoutedT t r m) where
  askStorage = lift askStorage
  tellStorage = lift . tellStorage

instance HasStorage t k m => HasStorage t k (EventWriterT t w m) where
  askStorage = lift askStorage
  tellStorage = lift . tellStorage
