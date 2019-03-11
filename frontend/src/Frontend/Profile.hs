{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
module Frontend.Profile where

import           Control.Lens
import           Reflex.Dom.Core

import           Data.Bool                               (bool)
import           Data.Functor                            (void)
import           Obelisk.Route.Frontend                  (pattern (:/), R,
                                                          RouteToUrl, RoutedT,
                                                          SetRoute, askRoute)
import           Servant.Common.Req                      (QParam (QNone),
                                                          reqSuccess)

import           Common.Route                            (FrontendRoute (..),
                                                          ProfileRoute (..),
                                                          Username (..))
import           Frontend.ArticlePreview                 (articlesPreview, profileImage)
import           Frontend.FrontendStateT
import           Frontend.Utils                          (buttonClass,
                                                          routeLinkDynClass)
import           RealWorld.Conduit.Api.Articles.Articles (Articles (..))
import           RealWorld.Conduit.Api.Namespace         (unNamespace)
import qualified RealWorld.Conduit.Api.User.Account      as Account
import           RealWorld.Conduit.Client                (apiArticles, apiUser,
                                                          articlesList,
                                                          getClient,
                                                          userCurrent)

profile
  :: ( DomBuilder t m
     , PostBuild t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     , HasFrontendState t s m
     , HasLoggedInAccount s
     , Prerender js m
     , TriggerEvent t m
     , PerformEvent t m
     , MonadHold t m
     )
  => Dynamic t Username
  -> RoutedT t (Maybe (R ProfileRoute)) m ()
profile usernameDyn =
  elClass "div" "profile-page" $ do
  elClass "div" "user-info" $
    elClass "div" "container" $
      elClass "div" "row" $
        elClass "div" "col-xs-12 col-md-10 offset-md-1" $ prerender (text "Loading") $ do
          tokDyn <- reviewFrontendState (loggedInAccount._Just.to Account.token)
          pbE <- getPostBuild
          loadResE <- getClient ^. apiUser . userCurrent . to (\f -> f
            (Identity <$> tokDyn)
            pbE
            )
          let loadSuccessE = fmap unNamespace . reqSuccess . runIdentity <$> loadResE
          void $ widgetHold (text "Loading") $ ffor loadSuccessE $
            maybe blank $ \acct -> do
              profileImage "user-img" (constDyn $ Account.image acct)
              el "h4" $ text $ Account.username acct
              el "p" $ text $ Account.bio acct
              _ <- buttonClass "btn btn-sm btn-outline-secondary action-btn" $ do
                elClass "i" "ion-plus-round" blank
                text " Follow "
                text $ Account.username acct
              pure ()
  elClass "div" "container" $
    elClass "div" "row" $
      elClass "div" "col-xs-12 col-md-10 offset-md-1" $ do
        elClass "div" "articles-toggle" $
          elClass "ul" "nav nav-pills outline-active" $ do
            rDyn <- askRoute
            navItem Nothing rDyn $ text "My Articles"
            navItem (Just $ ProfileRoute_Favourites :/ ()) rDyn $ text "My Favourites"
        prerender (text "Loading...") $ do
          tokDyn <- reviewFrontendState (loggedInAccount._Just.to Account.token)
          pbE <- getPostBuild
          artE <- getClient ^. apiArticles . articlesList . to (\f -> f
            (constDyn . Identity $ QNone)
            (constDyn . Identity $ QNone)
            (constDyn . Identity $ [])
            (Identity . pure . unUsername <$> usernameDyn)
            (constDyn . Identity $ [])
            --(Identity . pure . unUsername <$> usernameDyn)
            (Identity <$> tokDyn)
            (leftmost [pbE,void $ updated tokDyn])
            )
          artsDyn <- holdDyn (Articles [] 0) (fmapMaybe (reqSuccess . runIdentity) artE)
          articlesPreview artsDyn
  where
    navItem sr rDyn = elClass "li" "nav-item" . routeLinkDynClass
      (("nav-link " <>) . bool "" " active" . (== sr) <$> rDyn)
      ((\u -> FrontendRoute_Profile :/ (u,sr)) <$> usernameDyn)
