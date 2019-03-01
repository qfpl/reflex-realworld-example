{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE PatternSynonyms       #-}
module Frontend.Profile where

import           Reflex.Dom.Core

import           Data.Bool               (bool)
import qualified Data.Map                as Map
import           Obelisk.Route.Frontend  (pattern (:/), R, RouteToUrl, RoutedT,
                                          SetRoute, askRoute)

import           Common.Route            (FrontendRoute (..), ProfileRoute (..),
                                          Username(..), DocumentSlug(..))
import           Frontend.ArticlePreview (articlePreview)
import           Frontend.Utils          (routeLinkDynClass)

profile
  :: ( DomBuilder t m
     , MonadSample t m
     , PostBuild t m
     , SetRoute t (R FrontendRoute) m
     , RouteToUrl (R FrontendRoute) m
     )
  => Dynamic t Username
  -> RoutedT t (Maybe (R ProfileRoute)) m ()
profile usernameDyn = elClass "div" "profile-page" $ do
  elClass "div" "user-info" $
    elClass "div" "container" $
      elClass "div" "row" $
        elClass "div" "col-xs-12 col-md-10 offset-md-1" $ do
          elAttr "img" (Map.fromList [("src","http://i.imgur.com/Qr71crq.jpg"),("class","user-img")]) blank
          el "h4" $ text "Eric Simons"
          el "p" $ text "Cofounder @GoThinkster, lived in Aol's HQ for a few months, kinda looks like Peeta from the Hunger Games"
          (_,_) <- elClass' "button" "btn btn-sm btn-outline-secondary action-btn" $ do
            elClass "i" "ion-plus-round" blank
            text " Follow Eric Simons"
          pure ()
  elClass "div" "container" $
    elClass "div" "row" $
      elClass "div" "col-xs-12 col-md-10 offset-md-1" $ do
        elClass "div" "articles-toggle" $
          elClass "ul" "nav nav-pills outline-active" $ do
            rDyn <- askRoute
            navItem Nothing rDyn $ text "My Articles"
            navItem (Just $ ProfileRoute_Favourites :/ ()) rDyn $ text "My Favourites"
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

  where
    navItem sr rDyn = elClass "li" "nav-item" . routeLinkDynClass
      (("nav-link " <>) . bool "" " active" . (== sr) <$> rDyn)
      ((\u -> FrontendRoute_Profile :/ (u,sr)) <$> usernameDyn)
