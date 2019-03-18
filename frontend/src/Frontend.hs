{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PatternSynonyms            #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}

module Frontend where

import           Control.Lens
import           Reflex.Dom.Core hiding (Namespace)

import           Control.Monad.Trans.Reader          (mapReaderT)
import           Data.List.NonEmpty                  (NonEmpty((:|)))
import qualified Data.Map                            as Map
import           Data.Monoid                         (appEndo)
import           Data.Text                           (Text)
import           Obelisk.Frontend                    (Frontend (Frontend),
                                                      ObeliskWidget)
import           Obelisk.Route.Frontend              (pattern (:/), R,
                                                      RouteToUrl, RoutedT,
                                                      SetRoute, mapRoutedT,
                                                      subRoute_)
import           Reflex.Dom.Storage.Base             (StorageT (..),
                                                      StorageType (LocalStorage),
                                                      runStorageT)
import           Reflex.Dom.Storage.Class            (askStorageTag, pdmInsert,
                                                      pdmRemove, tellStorage)
import           Servant.Common.Req                  (ReqResult, reqSuccess)


import           Common.Route                        (FrontendRoute (..))
import           Frontend.Article                    (article)
import           Frontend.Editor                     (editor)
import           Frontend.FrontendStateT
import           Frontend.HomePage                   (homePage)
import           Frontend.LocalStorageKey            (LocalStorageTag (..))
import           Frontend.Login                      (login)
import           Frontend.Nav                        (nav)
import           Frontend.Profile                    (profile)
import           Frontend.Register                   (register)
import           Frontend.Settings                   (settings)
import           Frontend.Utils                      (pathSegmentSubRoute,
                                                      routeLinkClass)
import           RealWorld.Conduit.Api.Namespace     (Namespace, unNamespace)
import           RealWorld.Conduit.Api.User.Account (Account)
import qualified RealWorld.Conduit.Api.User.Account as Account
import           RealWorld.Conduit.Client            (apiUser, getClient,
                                                      userCurrent)

import System.Entropy
import Control.Monad.IO.Class (liftIO)

mapStorageT :: (forall x. m x -> n x) -> StorageT t k m a -> StorageT t k n a
mapStorageT f = StorageT . mapReaderT (mapEventWriterT f) . unStorageT

styleLink :: DomBuilder t m => Text -> m ()
styleLink href =
  elAttr "link" (Map.fromList [("href",href),("rel","stylesheet"),("type","text/css")]) blank

htmlHead :: (DomBuilder t m) => m ()
htmlHead = do
  el "title" $ text "Conduit"
  styleLink "//code.ionicframework.com/ionicons/2.0.1/css/ionicons.min.css"
  styleLink "//fonts.googleapis.com/css?family=Titillium+Web:700|Source+Serif+Pro:400,700|Merriweather+Sans:400,700|Source+Sans+Pro:400,300,600,700,300italic,400italic,600italic,700italic"
  styleLink "//demo.productionready.io/main.css"

type RoutedAppState t m = RoutedT t (R FrontendRoute) (AppState t m)

type AppState t m
  = EventWriterT t
    (NonEmpty FrontendEvent)
    (StorageT t LocalStorageTag (FrontendStateT t FrontendData m))


htmlBody
  :: forall t js m
  . ( ObeliskWidget t js (R FrontendRoute) m
    )
  => RoutedT t (R FrontendRoute) m ()
htmlBody = prerender (text "Loading...") $ mdo
  pbbE <- getPostBuild
  rndE <- performEvent $ ffor pbbE $ \_ -> liftIO $ getEntropy 10
  rndDyn <- holdDyn "" rndE
  display rndDyn
  mapRoutedT unravelAppState $ do
    jwtDyn <- askStorageTag LocalStorageJWT
    pbE <- getPostBuild
    currentUserE <- getClient ^. apiUser . userCurrent . to (\f -> f (Identity <$> jwtDyn) pbE)
    currentUserResUpdate currentUserE
    stateDyn <- askFrontendState
    nav ((view loggedInAccount) <$> stateDyn)
    subRoute_ pages
    footer
  pure ()
  where
    currentUserResUpdate
      :: Event t (Identity (ReqResult () (Namespace "user" Account)))
      -> RoutedAppState t m ()
    currentUserResUpdate =
      tellEvent
      . fmap ((:| []) . LogIn . unNamespace)
      . fmapMaybe (reqSuccess . runIdentity)

    unravelAppState :: AppState t m () -> m ()
    unravelAppState m = prerender (text "Loading...") $ mdo
      lsDyn <- foldDyn appEndo initialFrontendData (foldMap updateFrontendData <$> sE)
      sE <- flip runFrontendStateT lsDyn $ do
        runStorageT LocalStorage $ do
          (_, sInnerE) <- runEventWriterT m
          tellStorage (foldMap localEventToDMapPatch <$> sInnerE)
          pure sInnerE
      pure ()

    localEventToDMapPatch :: FrontendEvent -> PatchDMap LocalStorageTag Identity
    localEventToDMapPatch = \case
      LogOut  -> pdmRemove LocalStorageJWT
      LogIn a -> pdmInsert LocalStorageJWT (Account.token a)

    pages
      :: ( ObeliskWidget t x (R FrontendRoute) m
         )
      => FrontendRoute a
      -> RoutedT t a
         (EventWriterT t
           (NonEmpty FrontendEvent)
           (StorageT t LocalStorageTag (FrontendStateT t FrontendData m)))
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
