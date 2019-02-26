{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
module Frontend.Editor where

import           Reflex.Dom.Core

import qualified Data.Map               as Map
import           Obelisk.Route.Frontend (pattern (:/), R, RouteToUrl, SetRoute, routeLink)

import           Common.Route           (FrontendRoute (..), DocumentSlug)

editor
  :: ( DomBuilder t m
     , PostBuild t m
     , Prerender js m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     )
  => m ()
editor = elClass "div" "editor-page" $ do
  blank
