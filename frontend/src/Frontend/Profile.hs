{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
module Frontend.Profile where

import           Reflex.Dom.Core

import qualified Data.Map               as Map
import           Obelisk.Route.Frontend (pattern (:/), R, RouteToUrl, SetRoute, routeLink)

import           Common.Route           (FrontendRoute (..), Username)

profile
  :: ( DomBuilder t m
     , PostBuild t m
     , Prerender js m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     )
  => Dynamic t Username
  -> m ()
profile usernameDyn = elClass "div" "profile-page" $ do
  blank
