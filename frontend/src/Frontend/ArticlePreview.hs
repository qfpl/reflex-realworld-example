{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
module Frontend.ArticlePreview where


import           Reflex.Dom.Core

import           Data.Text              (Text)
import           Obelisk.Route          (pattern (:/), R)
import           Obelisk.Route.Frontend (RouteToUrl, SetRoute, routeLink)

import           Common.Route           (DocumentSlug, FrontendRoute (..),
                                         Username)
import           Frontend.Utils         (routeLinkClass)

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
    routeLink (FrontendRoute_Profile :/ (u,Nothing)) $ elAttr "img" ("src" =: h) blank
    elClass "div" "info" $ do
      routeLinkClass "author" (FrontendRoute_Profile :/ (u,Nothing)) $ text a
      elClass "span" "date" $ text dt
    elClass "button" "btn btn-outline-primary btn-sm pull-xs-right" $ do
      elClass "i" "ion-heart" blank
      text " "
      text "29"
    routeLinkClass "preview-link" (FrontendRoute_Article :/ s) $ do
      el "h1" $ text t
      el "p" $ text desc
      el "span" $ text "Read more..."
