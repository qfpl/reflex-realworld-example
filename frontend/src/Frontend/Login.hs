{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
module Frontend.Login where

import           Control.Lens
import           Reflex.Dom.Core

import           Control.Monad.IO.Class                  (MonadIO)
import           Data.Functor.Identity                   (runIdentity)
import           Data.List.NonEmpty                      (NonEmpty)
import qualified Data.Map                                as Map
import           Obelisk.Route.Frontend                  (pattern (:/), R,
                                                          RouteToUrl, SetRoute,
                                                          routeLink)
import           Servant.Common.Req                      (reqSuccess)

import           Common.Route                            (FrontendRoute (..))
import           Frontend.FrontendStateT
import           Frontend.Utils                          (buttonClass)
import           RealWorld.Conduit.Api.Namespace         (Namespace (Namespace),
                                                          unNamespace)
import           RealWorld.Conduit.Api.Users.Credentials (Credentials (Credentials))
import           RealWorld.Conduit.Client

login
  :: ( DomBuilder t m
     , PostBuild t m
     , Prerender js m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , TriggerEvent t m
     , PerformEvent t m
     , EventWriter t (NonEmpty e) m
     , AsFrontendEvent e
     , HasLoggedInAccount s
     , HasFrontendState t s m
     , MonadIO (Performable m)
     )
  => m ()
login = noUserWidget $ elClass "div" "auth-page" $ do
  elClass "div" "container-page" $
    elClass "div" "row" $
      elClass "div" "col-md-6 offset-md-3 col-xs-12" $ do
        elClass "h1" "text-xs-center" $ text "Sign in"
        elClass "p" "text-xs-center" $
          routeLink (FrontendRoute_Register :/ ()) $ text "Need an account?"
        elClass "ul" "error-messages" $
          blank
        prerender blank $ el "form" $ do
          emailI <- elClass "fieldset" "form-group" $
            textInput $ def
              & textInputConfig_attributes .~ constDyn (Map.fromList
                [ ("class","form-control form-control-lg")
                , ("placeholder","Email")
                ])
          passI <- elClass "fieldset" "form-group" $
            textInput $ def
              & textInputConfig_inputType  .~ "password"
              & textInputConfig_attributes .~ constDyn (Map.fromList
                [ ("class","form-control form-control-lg")
                , ("placeholder","Password")
                ])
          submitE <- buttonClass "btn btn-lg btn-primary pull-xs-right" $ text "Sign in"
          let credentials = Credentials
                <$> emailI ^. textInput_value
                <*> passI ^. textInput_value
          resE <- getClient ^. apiUsers . usersLogin . to (\f -> f (pure . pure . Namespace <$> credentials) submitE)
          tellEvent (fmap (pure . (_LogIn #) . unNamespace) . fmapMaybe (reqSuccess . runIdentity) $ resE)
          pure ()
  pure ()
