{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Frontend.Editor where

import           Reflex.Dom.Core

import qualified Data.Map               as Map
import           Obelisk.Route.Frontend (RoutedT)

import           Common.Route           (DocumentSlug)

editor
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , Prerender js m
     )
  => RoutedT t (Maybe DocumentSlug) m ()
editor = elClass "div" "editor-page" $ do
  elClass "div" "container" $
    elClass "div" "row" $
      elClass "div" "col-xs-12 col-md-10 offset-md-1" $ do
        prerender blank $ el "form" $
          el "fieldset" $ do
            _ <- elClass "fieldset" "form-group" $
              textInput $ def
                & textInputConfig_attributes .~ (constDyn (Map.fromList
                  [("class","form-control")
                  ,("placeholder","Article Title")
                  ]))
            _ <- elClass "fieldset" "form-group" $
              textInput $ def
                & textInputConfig_attributes .~ (constDyn (Map.fromList
                  [("class","form-control")
                  ,("placeholder","What's this article about?")
                  ]))
            _ <- elClass "fieldset" "form-group" $
              textArea $ def
                & textAreaConfig_attributes .~ (constDyn (Map.fromList
                  [("class","form-control")
                  ,("placeholder","Write your article (in markdown)")
                  ,("rows","8")
                  ]))
            (bElt,_) <- elClass' "button" "btn btn-lg btn-primary pull-xs-right" $ text "Publish Article"
            let publishE = domEvent Click bElt
            clicked :: Dynamic t Int <- count publishE
            display clicked
            pure ()
  pure ()
