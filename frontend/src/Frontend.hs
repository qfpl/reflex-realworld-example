{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeApplications      #-}
module Frontend where

import Control.Lens hiding (element)

import           Data.Bool              (bool)
import           Data.Dependent.Sum     (DSum ((:=>)))
import           Data.Functor           (void)
import qualified Data.Map               as Map
import           Data.Proxy             (Proxy(Proxy))
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

htmlBody :: (ObeliskWidget t x (R FrontendRoute) m) => RoutedT t (R FrontendRoute) m ()
htmlBody = do
  header (constDyn False)
  subRoute_ $ \case
    FrontendRoute_Home -> homePage
    _                  -> blank
  footer

-- This should be in obelisk, maybe? It's basically routeLink with a class passed
-- in as a dynamic
anchor
  :: forall t m a r
  .  ( DomBuilder t m
     , RouteToUrl r m
     , SetRoute t r m
     , PostBuild t m
     , MonadSample t m
     )
  => Dynamic t Text
  -> r
  -> m a
  -> m a
anchor cDyn r m = do
  enc <- askRouteToUrl
  initClass <- sample . current $ cDyn
  let initAttrs = (Map.fromList [("href",enc r),("class",initClass)])
  modAttrs <- dynamicAttributesToModifyAttributes (("class" =:) <$> cDyn)
  let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (\_ -> preventDefault)
        & elementConfig_initialAttributes .~ initAttrs
        & elementConfig_modifyAttributes  .~ modAttrs
  (e, a) <- element "a" cfg m
  setRoute $ r <$ domEvent Click e
  return a

data HeaderActive
  = HeaderActiveHome
  | HeaderActiveEditor
  | HeaderActiveSettings
  | HeaderActiveSignIn
  | HeaderActiveSignUp
  deriving Eq

header
  :: ( DomBuilder t m
     , PostBuild t m
     , MonadHold t m
     , Routed t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     )
  => Dynamic t Bool
  -> m ()
header loggedIn = do
  headerActive <- fmap calcSelected <$> askRoute
  elClass "nav" "navbar navbar-light" $
    elClass "div" "container" $ do
      anchor "navbar-brand" (FrontendRoute_Home :/ ()) $ text "conduit"
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
      elClass "li" "nav-item" . anchor (("nav-link " <>) . bool "" " active" . (== Just ha) <$> haDyn) href

articlePreview
  :: ( DomBuilder t m
     , PostBuild t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , MonadSample t m
     )
  => DocumentSlug -> Text -> Username -> Text -> Text -> Text -> Text -> m ()
articlePreview s h u a dt t desc = elClass "div" "article-preview" $ do
  elClass "div" "article-meta" $ do
    anchor (constDyn "") (FrontendRoute_Profile :/ u) $ elAttr "img" ("src" =: h) blank
    elClass "div" "info" $ do
      anchor (constDyn "author") (FrontendRoute_Profile :/ u) $ text a
      elClass "span" "date" $ text dt
    elClass "button" "btn btn-outline-primary btn-sm pull-xs-right" $ do
      elClass "i" "ion-heart" blank
      text " "
      text "29"
    anchor "preview-link" (FrontendRoute_Article :/ s) $ do
      el "h1" $ text t
      el "p" $ text desc
      el "span" $ text "Read more..."

homePage
  :: ( PostBuild t m
     , DomBuilder t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , MonadSample t m
     )
  => m ()
homePage = elClass "div" "home-page" $ do
  elClass "div" "banner" $
    elClass "div" "container" $ do
      elClass "h1" "logo-font" $ text "conduit"
      el "p" $ text "A place to share your knowledge"
  elClass "div" "container page" $ elClass "div" "row" $ do
    elClass "div" "col-md-9" $ do
      elClass "div" "feed-toggle" $
        elClass "ul" "nav nav-pills outline-active" $ do
          elClass "li" "nav-item" $ anchor (constDyn "nav-link disabled") (FrontendRoute_Home :/ ()) $ text "Your Feed"
          elClass "li" "nav-item" $ anchor (constDyn "nav-link active") (FrontendRoute_Home :/ ()) $ text "Global Feed"
      articlePreview
        (DocumentSlug "doc1")
        "http://i.imgur.com/Qr71crq.jpg"
        (Username "esimons")
        "Eric Simons"
        "January 20th"
        "How to build webapps that scale"
        "This is the description for the post."
      articlePreview
        (DocumentSlug "doc2")
        "http://i.imgur.com/N4VcUeJ.jpg"
        (Username "apai")
        "Albert Pai"
        "January 20th"
        "The song you won't ever stop singing. No matter how hard you try."
        "This is the description for the post."
    elClass "div" "col-md-3" $
      elClass "div" "sidebar" $ do
        el "p" $ text "Popular Tags"
        elClass "div" "tag-list" $ do
          tagPill "programming"
          tagPill "javascript"
          tagPill "emberjs"
          tagPill "angularjs"
          tagPill "react"
          tagPill "mean"
          tagPill "node"
          tagPill "rails"
  where
    tagPill t = anchor (constDyn "tag-pill tag-default") (FrontendRoute_Home :/ ()) $ text t

footer
  :: ( DomBuilder t m
     , PostBuild t m
     , RouteToUrl (R (FrontendRoute)) m
     , SetRoute t (R (FrontendRoute)) m
     , MonadSample t m
     )
  => m ()
footer = el "footer" $ elClass "div" "container" $ do
  anchor (constDyn "logo-font") (FrontendRoute_Home :/ ()) $ text "conduit"
  elClass "span" "attribution" $ do
    text "An interactive learning project from "
    elAttr "a" ("href" =: "https://thinkster.io") $ text "Thinkster"
    text ". Code & designed licensed under MIT."

frontend :: Frontend (R FrontendRoute)
frontend = Frontend htmlHead htmlBody
