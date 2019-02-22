{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
module Frontend where

import qualified Data.Map         as Map
import           Data.Text        (Text)
import qualified Data.Text        as T
import           Obelisk.Frontend
import           Obelisk.Route
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

htmlBody :: (DomBuilder t m) => m ()
htmlBody = do
  header
  homePage
  footer

anchor :: DomBuilder t m => Text -> Text -> m a -> m a
anchor c h m = elAttr "a" (Map.fromList [("class",c),("href",h)]) m

header :: (DomBuilder t m) => m ()
header = elClass "nav" "navbar navbar-light" $
  elClass "div" "container" $ do
    anchor "navbar-brand" "/" $ text "conduit"
    elClass "ul" "nav navbar-nav pull-xs-right" $ do
      elClass "li" "nav-item" $ anchor "navbar-link active" "/" $ text "Home"
      elClass "li" "nav-item" $ anchor "navbar-link" "/" $ do
        elClass "i" "ion-compose" blank
        text " "
        text "New Post"
      elClass "li" "nav-item" $ anchor "navbar-link" "/" $ do
        elClass "i" "ion-gear-a" blank
        text " "
        text "Settings"
      elClass "li" "nav-item" $ anchor "navbar-link" "/" $ do
        text "Sign up"

articlePreview :: DomBuilder t m => Text -> Text -> Text -> Text -> Text -> m ()
articlePreview h a dt t desc = elClass "div" "article-preview" $ do
  elClass "div" "article-meta" $ do
    anchor "" "/" $ elAttr "img" ("src" =: h) blank
    elClass "div" "info" $ do
      anchor "/" "author" $ text a
      elClass "span" "date" $ text dt
    elClass "button" "btn btn-outline-primary btn-sm pull-xs-right" $ do
      elClass "i" "ion-heart" blank
      text " "
      text "29"
    anchor "preview-link" "/" $ do
      el "h1" $ text t
      el "p" $ text desc
      el "span" $ text "Read more..."

homePage :: DomBuilder t m => m ()
homePage = elClass "div" "home-page" $ do
  elClass "div" "banner" $
    elClass "div" "container" $ do
      elClass "h1" "logo-font" $ text "conduit"
      el "p" $ text "A place to share your knowledge"
  elClass "div" "container page" $ elClass "div" "row" $ do
    elClass "div" "col-md-9" $ do
      elClass "div" "feed-toggle" $
        elClass "ul" "nav nav-pills outline-active" $ do
          elClass "li" "nav-item" $ anchor "nav-link disabled" "/" $ text "Your Feed"
          elClass "li" "nav-item" $ anchor "nav-link active" "/" $ text "Global Feed"
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
          anchor "tag-pill tag-default" "/" $ text "programming"
          anchor "tag-pill tag-default" "/" $ text "javascript"
          anchor "tag-pill tag-default" "/" $ text "emberjs"
          anchor "tag-pill tag-default" "/" $ text "angularjs"
          anchor "tag-pill tag-default" "/" $ text "react"
          anchor "tag-pill tag-default" "/" $ text "mean"
          anchor "tag-pill tag-default" "/" $ text "node"
          anchor "tag-pill tag-default" "/" $ text "rails"

footer :: (DomBuilder t m) => m ()
footer = el "footer" $ elClass "div" "container" $ do
  anchor "logo-font" "/" $ text "conduit"
  elClass "span" "attribution" $ do
    text "An interactive learning project from "
    elAttr "a" ("href" =: "https://thinkster.io") $ text "Thinkster"
    text ". Code & designed licensed under MIT."

frontend :: Frontend (R FrontendRoute)
frontend = Frontend htmlHead htmlBody
