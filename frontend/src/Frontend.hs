{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE TypeApplications           #-}

module Frontend where

import           Control.Lens
import           Reflex.Dom.Core

import           Data.List.NonEmpty                  (NonEmpty)
import qualified Data.Map                            as Map
import           Data.Monoid                         (appEndo)
import           Data.Text                           (Text)
import           Obelisk.Frontend                    (Frontend (Frontend),
                                                      ObeliskWidget)
import           Obelisk.Route.Frontend              (pattern (:/), R,
                                                      RouteToUrl, RoutedT,
                                                      SetRoute, mapRoutedT',
                                                      subRoute_)
import Reflex.Dom.Storage.Base (runStorageT)


import           Common.Route                        (FrontendRoute (..))
import           Frontend.Article                    (article)
import           Frontend.Editor                     (editor)
import           Frontend.FrontendStateT             (FrontendEvent,
                                                      FrontendStateT,
                                                      frontendStateLoggedInAccount,
                                                      initialFrontendState, updateFrontendState,
                                                      runFrontendStateT)
import           Frontend.HomePage                   (homePage)
import           Frontend.Login                      (login)
import           Frontend.Nav                        (nav)
import           Frontend.Profile                    (profile)
import           Frontend.Register                   (register)
import           Frontend.Settings                   (settings)
import           Frontend.Utils                      (pathSegmentSubRoute,
                                                      routeLinkClass)

styleLink :: DomBuilder t m => Text -> m ()
styleLink href =
  elAttr "link" (Map.fromList [("href",href),("rel","stylesheet"),("type","text/css")]) blank

htmlHead :: (DomBuilder t m) => m ()
htmlHead = do
  el "title" $ text "Conduit"
  styleLink "//code.ionicframework.com/ionicons/2.0.1/css/ionicons.min.css"
  styleLink "//fonts.googleapis.com/css?family=Titillium+Web:700|Source+Serif+Pro:400,700|Merriweather+Sans:400,700|Source+Sans+Pro:400,300,600,700,300italic,400italic,600italic,700italic"
  styleLink "//demo.productionready.io/main.css"

htmlBody
  :: ( ObeliskWidget t x (R FrontendRoute) m
     )
  => RoutedT t (R FrontendRoute) m ()
htmlBody = mdo
  stateDyn <- foldDyn appEndo initialFrontendState (foldMap updateFrontendState <$> stateEventsE)
  nav ((view frontendStateLoggedInAccount) <$> stateDyn)
  (_, stateEventsE) <- mapRoutedT' (flip runFrontendStateT stateDyn . runEventWriterT) (subRoute_ pages)
  footer
  where
    pages
      :: ( ObeliskWidget t x (R FrontendRoute) m
         )
      => FrontendRoute a
      -> RoutedT t a
          (EventWriterT t
            (NonEmpty FrontendEvent)
            (FrontendStateT t m))
           ()
    pages r = case r of
      FrontendRoute_Home     -> homePage
      FrontendRoute_Login    -> login
      FrontendRoute_Register -> register
      FrontendRoute_Article  -> article
      FrontendRoute_Settings -> settings
      FrontendRoute_Profile  -> pathSegmentSubRoute profile
      FrontendRoute_Editor   -> editor

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
