{-# LANGUAGE FlexibleContexts, GADTs, LambdaCase, MultiParamTypeClasses, OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms                                                               #-}
module Frontend.Profile where

import Control.Lens
import Reflex.Dom.Core

import Data.Bool              (bool)
import Data.Functor           (void)
import Obelisk.Route.Frontend (pattern (:/), R, RouteToUrl, RoutedT, SetRoute, askRoute)
import Servant.Common.Req     (QParam (QNone), reqSuccess)

import           Common.Conduit.Api.Articles.Articles (Articles (..))
import           Common.Conduit.Api.Namespace         (unNamespace)
import qualified Common.Conduit.Api.Profiles.Profile  as Profile
import qualified Common.Conduit.Api.User.Account      as Account
import           Common.Route                         (FrontendRoute (..), ProfileRoute (..), Username (..))
import           Frontend.ArticlePreview              (articlesPreview, profileImage)
import           Frontend.Conduit.Client              (apiArticles, apiProfile, articlesList, getClient,
                                                       profileGet)
import           Frontend.FrontendStateT
import           Frontend.Utils                       (buttonClass, routeLinkDynClass)

profile
  :: ( DomBuilder t m
     , PostBuild t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     , HasLoggedInAccount s
     , Prerender js t m
     , MonadHold t m
     , Monad (Client m)
     , HasFrontendState t s (Client m)
     , RouteToUrl (R FrontendRoute) (Client m)
     , SetRoute t (R FrontendRoute) (Client m)
     )
  => Dynamic t Username
  -> RoutedT t (Maybe (R ProfileRoute)) m ()
profile usernameDyn =
  elClass "div" "profile-page" $ do
  elClass "div" "user-info" $
    elClass "div" "container" $
      elClass "div" "row" $
        elClass "div" "col-xs-12 col-md-10 offset-md-1" $ prerender_ (text "Loading") $ do
          pbE <- getPostBuild
          loadResE <- getClient ^. apiProfile . profileGet . to (\f -> f
            (error "TODO: Fix prerendering")
            (Identity $ pure . unUsername <$> usernameDyn)
            (leftmost [pbE,void . updated $ usernameDyn])
            )
          let loadSuccessE = fmap unNamespace . reqSuccess . runIdentity <$> loadResE
          void $ widgetHold (text "Loading") $ ffor loadSuccessE $
            maybe blank $ \acct -> do
              profileImage "user-img" (constDyn $ Profile.image acct)
              el "h4" $ text $ Profile.username acct
              el "p" $ text $ Profile.bio acct
              _ <- buttonClass "btn btn-sm btn-outline-secondary action-btn" $ do
                elClass "i" "ion-plus-round" blank
                text " Follow "
                text $ Profile.username acct
              pure ()
  elClass "div" "container" $
    elClass "div" "row" $
      elClass "div" "col-xs-12 col-md-10 offset-md-1" $ do
        elClass "div" "articles-toggle" $
          elClass "ul" "nav nav-pills outline-active" $ do
            rDyn <- askRoute
            navItem Nothing rDyn $ text "My Articles"
            navItem (Just $ ProfileRoute_Favourites :/ ()) rDyn $ text "My Favourites"
        prerender_ (text "Loading...") $ do
          tokDyn <- reviewFrontendState (loggedInAccount._Just.to Account.token)
          pbE <- getPostBuild
          artE <- getClient ^. apiArticles . articlesList . to (\f -> f
            (Identity <$> tokDyn)
            (constDyn . Identity $ QNone)
            (constDyn . Identity $ QNone)
            (constDyn . Identity $ [])
            (Identity . pure . unUsername <$> usernameDyn)
            (constDyn . Identity $ [])
            (leftmost [pbE,void $ updated tokDyn])
            )
          artsDyn <- holdDyn (Articles [] 0) (fmapMaybe (reqSuccess . runIdentity) artE)
          articlesPreview artsDyn
  where
    navItem sr rDyn = elClass "li" "nav-item" . routeLinkDynClass
      (("nav-link " <>) . bool "" " active" . (== sr) <$> rDyn)
      ((\u -> FrontendRoute_Profile :/ (u,sr)) <$> usernameDyn)
