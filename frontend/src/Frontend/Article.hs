{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
module Frontend.Article where

import           Reflex.Dom.Core

import           Obelisk.Route.Frontend (Routed, askRoute)

import           Common.Route           (DocumentSlug)

article
  :: ( DomBuilder t m
     , PostBuild t m
     , Routed t DocumentSlug m
     )
  => m ()
article = elClass "div" "settings-page" $ do
  docSlugDyn <- askRoute
  display docSlugDyn
