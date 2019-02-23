{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
module Frontend where

import           Control.Monad.Fix      (MonadFix)
import           Data.Bool              (bool)
import           Data.Functor           (void)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Obelisk.Frontend
import           Obelisk.Route
import           Obelisk.Route.Frontend
import           Reflex.Dom.Core

import           Common.Route

styleLink :: DomBuilder t m => Text -> m ()
styleLink href =
  elAttr "link" (Map.fromList [("href",href),("rel","stylesheet"),("type","text/css")]) blank

htmlHead :: (DomBuilder t m) => m ()
htmlHead = do
  el "title" $ text "Conduit"
  styleLink "//code.ionicframework.com/ionicons/2.0.1/css/ionicons.min.css"
  styleLink "//fonts.googleapis.com/css?family=Titillium+Web:700|Source+Serif+Pro:400,700|Merriweather+Sans:400,700|Source+Sans+Pro:400,300,600,700,300italic,400italic,600italic,700italic"
  styleLink "//demo.productionready.io/main.css"

htmlBody :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m) => RoutedT t (R FrontendRoute) m ()
htmlBody = do
  header (constDyn False)
  subRoute_ $ \case
    FrontendRoute_Home -> homePage
    _                  -> blank
  footer

anchor :: (DomBuilder t m, PostBuild t m) => Dynamic t Text -> Text -> m a -> m a
anchor cDyn h m = elDynAttr "a" ((\c -> Map.fromList [("class",c),("href",h)]) <$> cDyn) m

data HeaderActive
  = HeaderActiveHome
  | HeaderActiveEditor
  | HeaderActiveSettings
  | HeaderActiveSignIn
  | HeaderActiveSignUp
  deriving Eq

header :: (DomBuilder t m, PostBuild t m, MonadFix m, MonadHold t m) => Dynamic t Bool -> RoutedT t (R FrontendRoute) m ()
header loggedIn = do
  headerActive <- subRoute calcSelected
  elClass "nav" "navbar navbar-light" $
    elClass "div" "container" $ do
      anchor "navbar-brand" "/" $ text "conduit"
      elClass "ul" "nav navbar-nav pull-xs-right" $ do
        navItem headerActive HeaderActiveHome "/" $ text "Home"
        void $ widgetHold
          (loggedOutMenu headerActive)
          (bool (loggedOutMenu headerActive) (loggedInMenu headerActive) <$> updated loggedIn)

  where
    loggedOutMenu haDyn = do
      navItem haDyn HeaderActiveSignIn "/" $ do
        text "Sign in"
      navItem haDyn HeaderActiveSignUp "/" $ do
        text "Sign up"

    loggedInMenu haDyn = do
      navItem haDyn HeaderActiveEditor "/" $ do
        elClass "i" "ion-compose" blank
        text " "
        text "New Post"
      navItem haDyn HeaderActiveSettings "/" $ do
        elClass "i" "ion-gear-a" blank
        text " "
        text "Settings"

    calcSelected :: Applicative m => FrontendRoute a -> RoutedT t a m (Maybe HeaderActive)
    calcSelected = \case
      FrontendRoute_Home     -> pure $ Just HeaderActiveHome
      FrontendRoute_Editor   -> pure $ Just HeaderActiveEditor
      FrontendRoute_Settings -> pure $ Just HeaderActiveEditor
      FrontendRoute_Login    -> pure $ Just HeaderActiveSignIn
      FrontendRoute_Register -> pure $ Just HeaderActiveSignUp
      _                      -> pure Nothing
    navItem haDyn ha href =
      elClass "li" "nav-item" . anchor (("nav-link " <>) . bool "" " active" . (== Just ha) <$> haDyn) href

articlePreview :: (DomBuilder t m, PostBuild t m) => Text -> Text -> Text -> Text -> Text -> m ()
articlePreview h a dt t desc = elClass "div" "article-preview" $ do
  elClass "div" "article-meta" $ do
    anchor (constDyn "") "/" $ elAttr "img" ("src" =: h) blank
    elClass "div" "info" $ do
      anchor (constDyn "author") "/" $ text a
      elClass "span" "date" $ text dt
    elClass "button" "btn btn-outline-primary btn-sm pull-xs-right" $ do
      elClass "i" "ion-heart" blank
      text " "
      text "29"
    anchor "preview-link" "/" $ do
      el "h1" $ text t
      el "p" $ text desc
      el "span" $ text "Read more..."

homePage :: (PostBuild t m, DomBuilder t m) => m ()
homePage = elClass "div" "home-page" $ do
  elClass "div" "banner" $
    elClass "div" "container" $ do
      elClass "h1" "logo-font" $ text "conduit"
      el "p" $ text "A place to share your knowledge"
  elClass "div" "container page" $ elClass "div" "row" $ do
    elClass "div" "col-md-9" $ do
      elClass "div" "feed-toggle" $
        elClass "ul" "nav nav-pills outline-active" $ do
          elClass "li" "nav-item" $ anchor (constDyn "nav-link disabled") "/" $ text "Your Feed"
          elClass "li" "nav-item" $ anchor (constDyn "nav-link active") "/" $ text "Global Feed"
      articlePreview
        "http://i.imgur.com/Qr71crq.jpg"
        "Eric Simons"
        "January 20th"
        "How to build webapps that scale"
        "This is the description for the post."
      articlePreview
        "http://i.imgur.com/N4VcUeJ.jpg"
        "Albert Pai"
        "January 20th"
        "The song you won't ever stop singing. No matter how hard you try."
        "This is the description for the post."
    elClass "div" "col-md-3" $
      elClass "div" "sidebar" $ do
        el "p" $ text "Popular Tags"
        elClass "div" "tag-list" $ do
          tagPill "/" "programming"
          tagPill "/" "javascript"
          tagPill "/" "emberjs"
          tagPill "/" "angularjs"
          tagPill "/" "react"
          tagPill "/" "mean"
          tagPill "/" "node"
          tagPill "/" "rails"
  where
    tagPill h t = anchor (constDyn "tag-pill tag-default") h $ text t

footer :: (DomBuilder t m, PostBuild t m) => m ()
footer = el "footer" $ elClass "div" "container" $ do
  anchor (constDyn "logo-font") "/" $ text "conduit"
  elClass "span" "attribution" $ do
    text "An interactive learning project from "
    elAttr "a" ("href" =: "https://thinkster.io") $ text "Thinkster"
    text ". Code & designed licensed under MIT."

frontend :: Frontend (R FrontendRoute)
frontend = Frontend htmlHead htmlBody
