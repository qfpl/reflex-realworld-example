{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
module Frontend.Profile where

import           Reflex.Dom.Core

import           Control.Monad.Fix      (MonadFix)
import           Data.Functor           (void)
import qualified Data.Map               as Map
import           Obelisk.Route.Frontend (pattern (:/), R, RouteToUrl, Routed,
                                         RoutedT, SetRoute, askRoute,
                                         maybeRoute, routeLink)

import           Common.Route           (FrontendRoute (..), ProfileRoute (..),
                                         Username)

profile
  :: ( DomBuilder t m
     , PostBuild t m
     , Prerender js m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , MonadFix m
     , MonadHold t m
     )
  => Dynamic t Username
  -> RoutedT t (Maybe (R ProfileRoute)) m ()
profile usernameDyn = elClass "div" "profile-page" $ do
  void $ maybeRoute (text "No favs") (text "Favs")
