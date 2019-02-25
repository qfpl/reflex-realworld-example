{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
module Frontend.Login where

import           Reflex.Dom.Core

import qualified Data.Map               as Map
import           Obelisk.Route.Frontend (pattern (:/), R, RouteToUrl, SetRoute, routeLink)

import           Common.Route           (FrontendRoute (..))

login
  :: ( DomBuilder t m
     , PostBuild t m
     , Prerender js m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     )
  => m ()
login = elClass "div" "auth-page" $ do
  elClass "div" "container-page" $
    elClass "div" "row" $
      elClass "div" "col-md-6 offset-md-3 col-xs-12" $ do
        elClass "h1" "text-xs-center" $ text "Sign in"
        elClass "p" "text-xs-center" $
          routeLink (FrontendRoute_Register :/ ()) $ text "Need an account?"
        elClass "ul" "error-messages" $
          blank
        prerender blank $ el "form" $ do
          _ <- elClass "fieldset" "form-group" $
            textInput $ def
              & textInputConfig_attributes .~ constDyn (Map.fromList
                [ ("class","form-control form-control-lg")
                , ("placeholder","Email")
                ])
          _ <- elClass "fieldset" "form-group" $
            textInput $ def
              & textInputConfig_inputType  .~ "password"
              & textInputConfig_attributes .~ constDyn (Map.fromList
                [ ("class","form-control form-control-lg")
                , ("placeholder","Password")
                ])
          (_,_) <- elClass' "button" "btn btn-lg btn-primary pull-xs-right" $ text "Sign in"
          pure ()
  pure ()
