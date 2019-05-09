{-# LANGUAGE FlexibleContexts, GADTs, LambdaCase, MultiParamTypeClasses, OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms                                                               #-}
module Frontend.Profile where

import Reflex.Dom.Core

import Control.Monad.Fix      (MonadFix)
import Data.Bool              (bool)
import Data.Functor           (void)
import Obelisk.Route.Frontend (pattern (:/), R, RouteToUrl, RoutedT, SetRoute, askRoute)
import Servant.Common.Req     (QParam (QNone))

import           Common.Conduit.Api.Articles.Articles (Articles (..))
import           Common.Conduit.Api.Namespace         (Namespace(Namespace))
import qualified Common.Conduit.Api.Profiles.Profile  as Profile
import           Common.Route                         (FrontendRoute (..), ProfileRoute (..), Username (..))
import           Frontend.ArticlePreview              (articlesPreview, profileImage)
import qualified Frontend.Conduit.Client              as Client
import           Frontend.FrontendStateT
import           Frontend.Utils                       (buttonClass, routeLinkDynClass)

profile
  :: ( DomBuilder t m
     , PostBuild t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     , HasLoggedInAccount s
     , HasFrontendState t s m
     , Prerender js t m
     , MonadHold t m
     , MonadFix m
     )
  => Dynamic t Username
  -> RoutedT t (Maybe (R ProfileRoute)) m ()
profile usernameDyn = do
  pbE <- getPostBuild
  tokDyn <- reviewFrontendState loggedInToken
  elClass "div" "profile-page" $ do
  elClass "div" "user-info" $
    elClass "div" "container" $
      elClass "div" "row" $
        elClass "div" "col-xs-12 col-md-10 offset-md-1" $ do
          (loadSuccessE,_,_) <- Client.getProfile
            tokDyn
            (pure . unUsername <$> usernameDyn)
            (leftmost [pbE,void . updated $ usernameDyn])

          void $ widgetHold (text "Loading") $ ffor loadSuccessE $ \(Namespace acct) -> do
            profileImage "user-img" (constDyn $ Profile.image acct)
            el "h4" $ text $ Profile.username acct
            el "p" $ text $ Profile.bio acct
            _ <- buttonClass "btn btn-sm btn-outline-secondary action-btn" (constDyn False) $ do
              elClass "i" "ion-plus-round" blank
              text " Follow "
              text $ Profile.username acct
            pure ()
  elClass "div" "container" $
    elClass "div" "row" $
      elClass "div" "col-xs-12 col-md-10 offset-md-1" $ do
        elClass "div" "articles-toggle" $ do
          elClass "ul" "nav nav-pills outline-active" $ do
            rDyn <- askRoute
            navItem Nothing rDyn $ text "My Articles"
            navItem (Just $ ProfileRoute_Favourites :/ ()) rDyn $ text "My Favourites"

          (loadArtsSuccessE,_,_) <- Client.listArticles
            tokDyn
            (constDyn QNone)
            (constDyn QNone)
            (constDyn [])
            (pure . unUsername <$> usernameDyn)
            (constDyn [])
            (leftmost [pbE,void $ updated tokDyn])

          artsDyn <- holdDyn (Articles [] 0) loadArtsSuccessE
          articlesPreview artsDyn
  where
    navItem sr rDyn = elClass "li" "nav-item" . routeLinkDynClass
      (("nav-link " <>) . bool "" " active" . (== sr) <$> rDyn)
      ((\u -> FrontendRoute_Profile :/ (u,sr)) <$> usernameDyn)
