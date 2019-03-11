{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Frontend.HomePage where

import           Control.Lens
import           Reflex.Dom.Core

import           Data.Functor                            (void)
import           Obelisk.Route                           (pattern (:/), R)
import           Obelisk.Route.Frontend                  (RouteToUrl, SetRoute)
import           Servant.Common.Req                      (QParam (QNone),
                                                          reqSuccess)

import           Common.Route                            (FrontendRoute (..))
import           Frontend.ArticlePreview                 (articlesPreview)
import           Frontend.FrontendStateT
import           Frontend.Utils                          (routeLinkClass)
import           RealWorld.Conduit.Api.Articles.Articles (Articles (..))
import           RealWorld.Conduit.Api.User.Account     (token)
import           RealWorld.Conduit.Client                (apiArticles,
                                                          articlesList,
                                                          getClient)

homePage
  :: forall t m s js
  . ( PostBuild t m
     , DomBuilder t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     --, MonadSample t m
     , MonadHold t m
     , TriggerEvent t m
     , PerformEvent t m
     , HasLoggedInAccount s
     , HasFrontendState t s m
     , Prerender js m
     )
  => m ()
homePage = prerender (text "Loading...") $ elClass "div" "home-page" $ do
  tokDyn <- reviewFrontendState (loggedInAccount._Just.to token)
  pbE <- getPostBuild
  artE <- getClient ^. apiArticles . articlesList . to (\f -> f
    (constDyn . Identity $ QNone)
    (constDyn . Identity $ QNone)
    (constDyn . Identity $ [])
    (constDyn . Identity $ [])
    (constDyn . Identity $ [])
    (Identity <$> tokDyn)
    (leftmost [pbE,void $ updated tokDyn])
    )
  artsDyn <- holdDyn (Articles [] 0) (fmapMaybe (reqSuccess . runIdentity) artE)
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
      articlesPreview artsDyn

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
