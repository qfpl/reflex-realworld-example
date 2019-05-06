{-# LANGUAGE FlexibleContexts, LambdaCase, MultiParamTypeClasses, OverloadedStrings, PatternSynonyms #-}
module Frontend.Login where

import Control.Lens
import Reflex.Dom.Core

import           Control.Monad.IO.Class (MonadIO)
import           Data.List.NonEmpty     (NonEmpty)
import qualified Data.Map               as Map
import           Obelisk.Route.Frontend (pattern (:/), R, RouteToUrl, SetRoute, routeLink)
import           Servant.Common.Req     (reqSuccess)

import           Common.Conduit.Api.Namespace         (Namespace (Namespace), unNamespace)
import           Common.Conduit.Api.Users.Credentials (Credentials (Credentials))
import           Common.Route                         (FrontendRoute (..))
import qualified Frontend.Conduit.Client              as Client
import           Frontend.FrontendStateT
import           Frontend.Utils                       (buttonClass)

login
  :: ( DomBuilder t m
     , PostBuild t m
     , Prerender js t m
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

        -- Put a link here that goes to signup
        elClass "p" "text-xs-center" $
          routeLink (FrontendRoute_Register :/ ()) $ text "Need an account?"

        elClass "ul" "error-messages" $
          blank

        -- A form for two inputs
        el "form" $ do
          emailI <- elClass "fieldset" "form-group" $
            inputElement $ def
              & inputElementConfig_elementConfig.elementConfig_initialAttributes .~ (Map.fromList
                [ ("class","form-control form-control-lg")
                , ("placeholder","Email")
                ])
          passI <- elClass "fieldset" "form-group" $
            inputElement $ def
              & inputElementConfig_elementConfig.elementConfig_initialAttributes .~ (Map.fromList
                [ ("class","form-control form-control-lg")
                , ("placeholder","Password")
                , ("type","password")
                ])
          -- And a submit button. Not really a submit element. Should fix this
          submitE <- buttonClass "btn btn-lg btn-primary pull-xs-right" $ text "Sign in"

          -- We take the Dynamics from the two inputs and make a Dynamic t Credentials
          -- Dynamic has an applicative instance.
          let credentials = Credentials
                <$> emailI ^. to _inputElement_value
                <*> passI ^. to _inputElement_value

          -- Do a backend call with the Dynamic t Credentials and the Submit Click
          resE <- Client.login
            (pure . Namespace <$> credentials)
            submitE

          -- We do some work to ignore failures and unwrap the result
          let successE = fmapMaybe (fmap unNamespace . reqSuccess) $ resE

          -- When we have a success fire off, we fire a login up the state tree
          -- This sets the JWT token into our state and local storage
          tellEvent $ pure . (_LogIn #) <$> successE

          pure ()
  pure ()
