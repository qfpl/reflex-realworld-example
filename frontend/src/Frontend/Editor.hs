{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
module Frontend.Editor where

import           Reflex.Dom.Core

import           Control.Monad.Fix      (MonadFix)
import           Obelisk.Route.Frontend (Routed, RoutedT, askRoute, maybeRoute_)

import           Common.Route           (DocumentSlug)

editor
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadFix m
     , MonadHold t m
     )
  => RoutedT t (Maybe DocumentSlug) m ()
editor = maybeRoute_ create edit

create
  :: ( DomBuilder t m
     )
  => m ()
create = elClass "div" "editor-page" $ do
  blank

edit
  :: ( DomBuilder t m
     , PostBuild t m
     , Routed t DocumentSlug m
     )
  => m ()
edit = elClass "div" "editor-page" $ do
  slugDyn <- askRoute
  display slugDyn
