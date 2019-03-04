{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Frontend.Settings where

import           Reflex.Dom.Core

import qualified Data.Map        as Map

settings
  :: ( DomBuilder t m
     , PostBuild t m
     , Prerender js m
     , MonadHold t m
     )
  => m ()
settings = elClass "div" "settings-page" $ do
  elClass "div" "container page" $
    elClass "div" "row" $
      elClass "div" "col-md-6 offset-md-3 col-xs-12" $ do
        elClass "h1" "text-xs-center" $ text "Your Settings"
        prerender blank $ el "form" $
          el "fieldset" $ do
            _ <- elClass "fieldset" "form-group" $
              textInput $ def
                & textInputConfig_attributes .~ (constDyn (Map.fromList
                  [("class","form-control")
                  ,("placeholder","URL of profile picture")
                  ]))
            _ <- elClass "fieldset" "form-group" $
              textInput $ def
                & textInputConfig_attributes .~ (constDyn (Map.fromList
                  [("class","form-control")
                  ,("placeholder","Your name")
                  ]))
            _ <- elClass "fieldset" "form-group" $
              textArea $ def
                & textAreaConfig_attributes .~ (constDyn (Map.fromList
                  [("class","form-control")
                  ,("placeholder","Short bio about you")
                  ,("rows","8")
                  ]))

            _ <- elClass "fieldset" "form-group" $
              textInput $ def
                & textInputConfig_inputType .~ "email"
                & textInputConfig_attributes .~ (constDyn (Map.fromList
                  [("class","form-control")
                  ,("placeholder","Email")
                  ]))
            _ <- elClass "fieldset" "form-group" $
              textInput $ def
                & textInputConfig_inputType .~ "password"
                & textInputConfig_attributes .~ (constDyn (Map.fromList
                  [("class","form-control")
                  ,("placeholder","Password")
                  ]))
            (bElt,_) <- elClass' "button" "btn btn-lg btn-primary pull-xs-right" $ text "Update Settings"
            let updateE = domEvent Click bElt
            clicked :: Dynamic t Int <- count updateE
            display clicked
            pure ()
