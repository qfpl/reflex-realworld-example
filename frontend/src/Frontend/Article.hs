{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE RecursiveDo           #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module Frontend.Article where

import           Reflex.Dom.Core

import           Control.Monad.Fix      (MonadFix)
import           Data.Text              (Text)
import           JSDOM.Document         (createElement)
import           JSDOM.Element          (setInnerHTML)
import           JSDOM.Types            (liftJSM)
import           Obelisk.Route.Frontend (pattern (:/), R, RouteToUrl, Routed,
                                         SetRoute, askRoute, routeLink)

import           Common.Route           (DocumentSlug, FrontendRoute (..),
                                         Username (..))
import           Frontend.Utils         (routeLinkClass)

article
  :: ( DomBuilder t m
     , Routed t DocumentSlug m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     , PostBuild t m
     , MonadHold t m
     , MonadFix m
     , Prerender js m
     , HasDocument m
     )
  => m ()
article = elClass "div" "article-page" $ do
  _ <- askRoute
  let htmlT :: Text = "<h1>Hello world</h1><p>This is some text</p>"
  elClass "div" "banner" $
    elClass "div" "container" $ do
      el "h1" $ text "How to build webapps that scale"
      articleMeta
  elClass "div" "container page" $ do
    elClass "div" "row article-content" $ prerender (text "Loading") $ do
      d <- askDocument
      e <- liftJSM $ do
        -- This wont execute scripts, but will allow users to XSS attack through
        -- event handling javascript attributes in any raw HTML that is let
        -- through the markdown renderer. But this is the simplest demo that
        -- mostly works. See https://github.com/qfpl/reflex-dom-template for a
        -- potentially more robust solution (we could filter out js handler attrs
        -- with something like that).
        e <- createElement d ("div" :: String)
        setInnerHTML e htmlT
        pure e
      placeRawElement e
    el "hr" blank
    elClass "div" "row article-actions"
      articleMeta

  where
    articleMeta = elClass "div" "article-meta" $ do
      let authorRoute = FrontendRoute_Profile :/ (Username "foo", Nothing)
      routeLink authorRoute $ elAttr "img" ("src" =: "http://i.imgur.com/Qr71crq.jpg") blank
      elClass "div" "info" $ do
        routeLinkClass "author" authorRoute $ text "Eric Simons"
        elClass "span" "date" $ text "January 20th"
      actions
    actions = do
      rec follows <- foldDyn ($) (10::Int) ((+1) <$ followClick)
          (followElt,_) <- elClass' "button" "btn btn-sm btn-outline-secondary action-btn" $ do
            elClass "i" "ion-plus-round" blank
            text " Follow Eric Simons"
            elClass "span" "counter" $ display follows
          let followClick = domEvent Click followElt
      rec favourites <- foldDyn ($) (22::Int) ((+1) <$ favouriteClick)
          (favouriteElt,_) <- elClass' "button" "btn btn-sm btn-outline-primary action-btn" $ do
            elClass "i" "ion-plus-round" blank
            text " Follow Eric Simons"
            elClass "span" "counter" $ display favourites
          let favouriteClick = domEvent Click favouriteElt
      pure ()
