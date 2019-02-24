{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE TypeFamilies          #-}
module Frontend.Nav where

import           Reflex.Dom.Core

import           Data.Bool              (bool)
import           Data.Dependent.Sum     (DSum ((:=>)))
import           Data.Functor           (void)
import           Obelisk.Route          (pattern (:/), R)
import           Obelisk.Route.Frontend (RouteToUrl, Routed, SetRoute, askRoute)

import           Common.Route           (FrontendRoute (..))
import           Frontend.Utils         (routeLinkDynClass)

data HeaderActive
  = HeaderActiveHome
  | HeaderActiveEditor
  | HeaderActiveSettings
  | HeaderActiveSignIn
  | HeaderActiveSignUp
  deriving Eq

nav
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , Routed t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     )
  => Dynamic t Bool
  -> m ()
nav loggedIn = do
  headerActive <- fmap calcSelected <$> askRoute
  elClass "nav" "navbar navbar-light" $
    elClass "div" "container" $ do
      routeLinkDynClass "navbar-brand" (FrontendRoute_Home :/ ()) $ text "conduit"
      elClass "ul" "nav navbar-nav pull-xs-right" $ do
        navItem headerActive HeaderActiveHome (FrontendRoute_Home :/ ()) $ text "Home"
        void $ widgetHold
          (loggedOutMenu headerActive)
          (bool (loggedOutMenu headerActive) (loggedInMenu headerActive) <$> updated loggedIn)

  where
    loggedOutMenu haDyn = do
      navItem haDyn HeaderActiveSignIn (FrontendRoute_Login :/ ()) $ do
        text "Sign in"
      navItem haDyn HeaderActiveSignUp (FrontendRoute_Register :/ ()) $ do
        text "Sign up"

    loggedInMenu haDyn = do
      navItem haDyn HeaderActiveEditor (FrontendRoute_Editor :/ Nothing) $ do
        elClass "i" "ion-compose" blank
        text " "
        text "New Post"
      navItem haDyn HeaderActiveSettings (FrontendRoute_Settings :/ ()) $ do
        elClass "i" "ion-gear-a" blank
        text " "
        text "Settings"

    calcSelected :: R FrontendRoute -> Maybe HeaderActive
    calcSelected = \case
      (FrontendRoute_Home :=> _)     -> Just HeaderActiveHome
      (FrontendRoute_Editor :=> _)   -> Just HeaderActiveEditor
      (FrontendRoute_Settings :=> _) -> Just HeaderActiveEditor
      (FrontendRoute_Login :=> _)    -> Just HeaderActiveSignIn
      (FrontendRoute_Register :=> _) -> Just HeaderActiveSignUp
      _                              -> Nothing
    navItem haDyn ha href =
      elClass "li" "nav-item" . routeLinkDynClass (("nav-link " <>) . bool "" " active" . (== Just ha) <$> haDyn) href
