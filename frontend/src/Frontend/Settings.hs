{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}

module Frontend.Settings where

import           Control.Lens
import           Reflex.Dom.Core

import           Control.Monad                      (mfilter)
import           Control.Monad.IO.Class             (MonadIO)
import           Data.List.NonEmpty                 (NonEmpty)
import qualified Data.Map                           as Map
import           Data.Maybe                         (fromMaybe)
import qualified Data.Text                          as Text
import           Obelisk.Route.Frontend             (R, SetRoute, setRoute, pattern (:/))
import           Servant.Common.Req                 (reqSuccess)

import           Common.Route                       (FrontendRoute(..), Username(..))
import           Frontend.FrontendStateT
import           Frontend.Utils                     (buttonClass)
import           RealWorld.Conduit.Api.Namespace    (Namespace(Namespace), unNamespace)
import qualified RealWorld.Conduit.Api.User.Account as Account
import           RealWorld.Conduit.Api.User.Update  (UpdateUser (UpdateUser))
import           RealWorld.Conduit.Client           (getClient)
import           RealWorld.Conduit.Client           (apiUser, userCurrent, userUpdate)

settings
  :: ( DomBuilder t m
     , PostBuild t m
     , Prerender js m
     , TriggerEvent t m
     , PerformEvent t m
     , MonadIO (Performable m)
     , SetRoute t (R FrontendRoute) m
     , EventWriter t (NonEmpty FrontendEvent) m
     , HasFrontendState t s m
     , HasLoggedInAccount s
     )
  => m ()
-- First we should look at userWidget !
settings = userWidget $ \acct -> elClass "div" "settings-page" $ do
  elClass "div" "container page" $
    elClass "div" "row" $
      elClass "div" "col-md-6 offset-md-3 col-xs-12" $ do
        elClass "h1" "text-xs-center" $ text "Your Settings"
        prerender blank $ el "form" $ do
          -- When this FRP network is built, we want to load the existing data
          pbE <- getPostBuild
          -- The only input is the JWT token that we load from the FrontendState
          let tokenDyn = constDyn . Identity . pure . Account.token $ acct
          loadResE <- getClient ^. apiUser . userCurrent . to (\f -> f
            tokenDyn
            pbE
            )
          -- Again, we throw away errors for now. We should fix this.
          let loadSuccessE = fmapMaybe (fmap unNamespace . reqSuccess . runIdentity) loadResE
          el "fieldset" $ do
            urlI <- elClass "fieldset" "form-group" $
              textInput $ def
                & textInputConfig_attributes .~ (constDyn (Map.fromList
                  [("class","form-control")
                  ,("placeholder","URL of profile picture")
                  ]))
                -- Note that we set the form val from AJAX returned data
                & textInputConfig_setValue .~ (fromMaybe "" . Account.image <$> loadSuccessE)
            usernameI <- elClass "fieldset" "form-group" $
              textInput $ def
                & textInputConfig_attributes .~ (constDyn (Map.fromList
                  [("class","form-control")
                  ,("placeholder","Your name")
                  ]))
                & textInputConfig_setValue .~ (Account.username <$> loadSuccessE)
            bioI <- elClass "fieldset" "form-group" $
              textArea $ def
                & textAreaConfig_attributes .~ (constDyn (Map.fromList
                  [("class","form-control")
                  ,("placeholder","Short bio about you")
                  ,("rows","8")
                  ]))
                & textAreaConfig_setValue .~ (Account.bio <$> loadSuccessE)

            emailI <- elClass "fieldset" "form-group" $
              textInput $ def
                & textInputConfig_inputType .~ "email"
                & textInputConfig_attributes .~ (constDyn (Map.fromList
                  [("class","form-control")
                  ,("placeholder","Email")
                  ]))
                & textInputConfig_setValue .~ (Account.email <$> loadSuccessE)
            passwordI <- elClass "fieldset" "form-group" $
              textInput $ def
                & textInputConfig_inputType .~ "password"
                & textInputConfig_attributes .~ (constDyn (Map.fromList
                  [("class","form-control")
                  ,("placeholder","Password")
                  ]))
            updateE <- buttonClass "btn btn-lg btn-primary pull-xs-right" $ text "Update Settings"
            -- Here we dont want to update the password if it was left blank
            let updateDyn = UpdateUser
                  <$> (mfilter (not . Text.null) . Just <$> passwordI ^. textInput_value)
                  <*> (Just <$> emailI ^. textInput_value)
                  <*> (Just <$> usernameI ^. textInput_value)
                  <*> (Just <$> bioI ^. textArea_value)
                  <*> (Just <$> urlI ^. textInput_value)

            -- Make the backend call when the submit button is clicked 
            -- and we have a valid UpdateUser
            updateResE <- getClient ^. apiUser . userUpdate . to (\f -> f
              tokenDyn
              (pure . pure . Namespace <$> updateDyn)
              updateE)
            
            -- More throwing away errors
            let updateSuccessE = fmapMaybe (fmap unNamespace . reqSuccess . runIdentity) updateResE
            
            -- Once we have updated successfully, we redirect to the profile page.
            setRoute $
              (\newA ->
                FrontendRoute_Profile :/ (Username $ Account.username newA, Nothing)
              ) <$> updateSuccessE
          
          el "hr" blank
          -- Add a logout button that dispatches a logout event.
          logoutClick <- buttonClass "btn btn-outline-danger" $ text "Logout"
          tellEvent $ pure LogOut <$ logoutClick
          -- And redirect to home.
          setRoute $ FrontendRoute_Home :/ () <$ logoutClick
          pure ()
