{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
module Frontend.Settings where

import           Reflex.Dom.Core

settings
  :: ( DomBuilder t m
     )
  => m ()
settings = elClass "div" "settings-page" $ do
  blank
