{-# LANGUAGE FlexibleContexts, LambdaCase, MultiParamTypeClasses, OverloadedStrings, PatternSynonyms #-}
module Frontend.Register where

import Control.Lens
import Reflex.Dom.Core


import           Data.List.NonEmpty     (NonEmpty)
import qualified Data.Map               as Map
import           Obelisk.Route.Frontend (pattern (:/), R, RouteToUrl, SetRoute, routeLink)
import           Servant.Common.Req     (reqSuccess)

import           Common.Conduit.Api.Namespace        (Namespace (Namespace), unNamespace)
import           Common.Conduit.Api.Users.Registrant (Registrant (Registrant))
import           Common.Route                        (FrontendRoute (..))
import qualified Frontend.Conduit.Client             as Client
import           Frontend.FrontendStateT
import           Frontend.Utils                      (buttonClass)


register
  :: ( DomBuilder t m
     , PostBuild t m
     , Prerender js t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , EventWriter t (NonEmpty e) m
     , AsFrontendEvent e
     , HasFrontendState t s m
     , HasLoggedInAccount s
     )
  => m ()
register = noUserWidget $ elClass "div" "auth-page" $ do
  elClass "div" "container-page" $
    elClass "div" "row" $
      elClass "div" "col-md-6 offset-md-3 col-xs-12" $ do
        elClass "h1" "text-xs-center" $ text "Sign up"
        elClass "p" "text-xs-center" $
          routeLink (FrontendRoute_Login :/ ()) $ text "Have an account?"
        elClass "ul" "error-messages" $
          blank
        el "form" $ do
          usernameI <- elClass "fieldset" "form-group" $
            inputElement $ def
              & inputElementConfig_elementConfig.elementConfig_initialAttributes .~ Map.fromList
                [ ("class","form-control form-control-lg")
                , ("placeholder","Your name")
                ]
          emailI <- elClass "fieldset" "form-group" $
            inputElement $ def
              & inputElementConfig_elementConfig.elementConfig_initialAttributes .~ Map.fromList
                [ ("class","form-control form-control-lg")
                , ("placeholder","Email")
                ]
          passI <- elClass "fieldset" "form-group" $
            inputElement $ def
              & inputElementConfig_elementConfig.elementConfig_initialAttributes .~ Map.fromList
                [ ("class","form-control form-control-lg")
                , ("placeholder","Password")
                , ("type","password")
                ]
          submitE <- buttonClass "btn btn-lg btn-primary pull-xs-right" (constDyn False) $ text "Sign Up"
          let registrant = Registrant
                <$> usernameI ^. to _inputElement_value
                <*> emailI ^. to _inputElement_value
                <*> passI ^. to _inputElement_value
          resE <- Client.register (pure . Namespace <$> registrant) submitE
          tellEvent (fmap (pure . (_LogIn #) . unNamespace) . fmapMaybe reqSuccess $ resE)
          pure ()
  pure ()
