{-# LANGUAGE FlexibleContexts, LambdaCase, MultiParamTypeClasses, OverloadedStrings, PatternSynonyms #-}
{-# LANGUAGE TypeFamilies                                                                            #-}
module Frontend.Nav where

import Reflex.Dom.Core

import Data.Bool              (bool)
import Data.Functor           (void)
import Obelisk.Route          (pattern (:/), R)
import Obelisk.Route.Frontend (RouteToUrl, Routed, SetRoute, askRoute)

import qualified Common.Conduit.Api.User.Account as Account
import           Common.Route                    (FrontendRoute (..), Username (..))
import           Frontend.FrontendStateT
import           Frontend.Utils                  (routeLinkDynClass)

nav
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , Routed t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , HasFrontendState t s m
     , HasLoggedInAccount s
     )
  => m ()
nav = do
  rDyn <- askRoute
  loggedIn <- reviewFrontendState loggedInAccount
  elClass "nav" "navbar navbar-light" $
    elClass "div" "container" $ do
      routeLinkDynClass "navbar-brand" (constDyn $ FrontendRoute_Home :/ ()) $ text "conduit"
      elClass "ul" "nav navbar-nav pull-xs-right" $ do
        navItem (FrontendRoute_Home :/ ()) rDyn $ text "Home"
        void $ widgetHold
          loggedOutMenu
          (maybe loggedOutMenu loggedInMenu <$> updated loggedIn)

  where
    loggedOutMenu = do
      rDyn <- askRoute
      navItem (FrontendRoute_Login :/ ()) rDyn $ do
        text "Sign in"
      navItem (FrontendRoute_Register :/ ()) rDyn $ do
        text "Sign up"

    loggedInMenu a = do
      rDyn <- askRoute
      navItem (FrontendRoute_Editor :/ Nothing) rDyn $ do
        elClass "i" "ion-compose" blank
        text " "
        text "New Post"
      navItem (FrontendRoute_Settings :/ ()) rDyn $ do
        elClass "i" "ion-gear-a" blank
        text " "
        text "Settings"
      navItem
        (FrontendRoute_Profile :/ (Username (Account.username a),Nothing))
        rDyn $ text $ Account.username a

    navItem r rDyn = elClass "li" "nav-item" . routeLinkDynClass
      (("nav-link " <>) . bool "" " active" . (== r) <$> rDyn)
      (constDyn r)
