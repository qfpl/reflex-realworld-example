{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
module Frontend where

import           Reflex.Dom.Core

import qualified Data.Map               as Map
import           Data.Text              (Text)
import           Obelisk.Frontend       (Frontend (Frontend), ObeliskWidget)
import           Obelisk.Route.Frontend (pattern (:/), R, RouteToUrl, RoutedT,
                                         SetRoute, subRoute_)

import           Common.Route           (FrontendRoute (..))
import           Frontend.HomePage      (homePage)
import           Frontend.Nav           (nav)
import           Frontend.Utils         (routeLinkClass)

styleLink :: DomBuilder t m => Text -> m ()
styleLink href =
  elAttr "link" (Map.fromList [("href",href),("rel","stylesheet"),("type","text/css")]) blank

htmlHead :: (DomBuilder t m) => m ()
htmlHead = do
  el "title" $ text "Conduit"
  styleLink "//code.ionicframework.com/ionicons/2.0.1/css/ionicons.min.css"
  styleLink "//fonts.googleapis.com/css?family=Titillium+Web:700|Source+Serif+Pro:400,700|Merriweather+Sans:400,700|Source+Sans+Pro:400,300,600,700,300italic,400italic,600italic,700italic"
  styleLink "//demo.productionready.io/main.css"

htmlBody :: (ObeliskWidget t x (R FrontendRoute) m) => RoutedT t (R FrontendRoute) m ()
htmlBody = do
  nav (constDyn False)
  subRoute_ $ \case
    FrontendRoute_Home -> homePage
    _                  -> blank
  footer

footer
  :: ( DomBuilder t m
     , PostBuild t m
     , RouteToUrl (R (FrontendRoute)) m
     , SetRoute t (R (FrontendRoute)) m
     , MonadSample t m
     )
  => m ()
footer = el "footer" $ elClass "div" "container" $ do
  routeLinkClass "logo-font" (FrontendRoute_Home :/ ()) $ text "conduit"
  elClass "span" "attribution" $ do
    text "An interactive learning project from "
    elAttr "a" ("href" =: "https://thinkster.io") $ text "Thinkster"
    text ". Code & designed licensed under MIT."

frontend :: Frontend (R FrontendRoute)
frontend = Frontend htmlHead htmlBody
