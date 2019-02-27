{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
module Frontend.Profile where

import           Reflex.Dom.Core

import           Control.Monad.Fix      (MonadFix)
import           Obelisk.Route.Frontend (R, RoutedT, maybeRoute_, subRoute_)

import           Common.Route           (ProfileRoute (..), Username)

profile
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     )
  => Dynamic t Username
  -> RoutedT t (Maybe (R ProfileRoute)) m ()
profile usernameDyn = elClass "div" "profile-page" $ do
  display usernameDyn
  maybeRoute_ (text "Standard") $ subRoute_ $ \case
    ProfileRoute_Favourites -> text "Favourites"
