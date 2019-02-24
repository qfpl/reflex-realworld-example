{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
module Frontend.HomePage where

import           Reflex.Dom.Core

import           Obelisk.Route           (pattern (:/), R)
import           Obelisk.Route.Frontend  (RouteToUrl, SetRoute)

import           Common.Route            (DocumentSlug (..), FrontendRoute (..),
                                          Username (..))
import           Frontend.ArticlePreview (articlePreview)
import           Frontend.Utils          (routeLinkClass)

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
          elClass "li" "nav-item" $ routeLinkClass "nav-link disabled" (FrontendRoute_Home :/ ()) $ text "Your Feed"
          elClass "li" "nav-item" $ routeLinkClass "nav-link active" (FrontendRoute_Home :/ ()) $ text "Global Feed"
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
    tagPill t = routeLinkClass "tag-pill tag-default" (FrontendRoute_Home :/ ()) $ text t
