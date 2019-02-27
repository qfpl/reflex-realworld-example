{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
module Frontend.Profile where

import           Reflex.Dom.Core

import           Obelisk.Route.Frontend (R, Routed, askRoute)

import           Common.Route           (ProfileRoute (..), Username)

profile
  :: ( DomBuilder t m
     , PostBuild t m
     , Routed t (Username, Maybe (R ProfileRoute)) m
     )
  => m ()
profile = elClass "div" "profile-page" $ do
  tupleDyn <- askRoute
  let username = fst <$> tupleDyn
  let sub      = snd <$> tupleDyn
  display username
  display sub
