{-# LANGUAGE FlexibleContexts, LambdaCase, MultiParamTypeClasses, OverloadedStrings, PatternSynonyms #-}
module Frontend.ArticlePreview where


import Reflex.Dom.Core

import           Control.Monad.Fix      (MonadFix)
import           Data.Functor           (void)
import qualified Data.Map               as Map
import           Data.Text              (Text)
import           Obelisk.Route          (pattern (:/), R)
import           Obelisk.Route.Frontend (RouteToUrl, SetRoute)

import Common.Route   (DocumentSlug (..), FrontendRoute (..), Username (..))
import Frontend.Utils (imgUrl, routeLinkDyn, routeLinkDynClass)

import           Common.Conduit.Api.Articles.Article  (Article)
import qualified Common.Conduit.Api.Articles.Article  as Article
import           Common.Conduit.Api.Articles.Articles (Articles)
import qualified Common.Conduit.Api.Articles.Articles as Articles
import qualified Common.Conduit.Api.Profiles.Profile  as Profile

articlesPreview
  :: ( DomBuilder t m
     , PostBuild t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , MonadHold t m
     , MonadFix m
     )
  => Dynamic t Articles -> m ()
articlesPreview artsDyn = do
  let artMapDyn = Map.fromList . fmap (\a -> (Article.id a, a)) . Articles.articles <$> artsDyn
  void $ list artMapDyn $ articlePreview

articlePreview
  :: ( DomBuilder t m
     , PostBuild t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , MonadSample t m
     )
  => Dynamic t Article -> m ()
articlePreview articleDyn = elClass "div" "article-preview" $ do
  elClass "div" "article-meta" $ do
    let profileDyn = Article.author <$> articleDyn
    let userRouteDyn = (\p ->
          (FrontendRoute_Profile :/ (Username . Profile.username $ p,Nothing))
         ) <$> profileDyn
    routeLinkDyn userRouteDyn $ profileImage "" (Profile.image <$> profileDyn)
    elClass "div" "info" $ do
      routeLinkDynClass (constDyn "author") userRouteDyn $ dynText (Profile.username <$> profileDyn)
      elClass "span" "date" $ display (Article.createdAt <$> articleDyn)
    elClass "button" "btn btn-outline-primary btn-sm pull-xs-right" $ do
      elClass "i" "ion-heart" blank
      text " "
      display $ Article.favoritesCount <$> articleDyn
    routeLinkDynClass (constDyn "preview-link")
      ((\a -> FrontendRoute_Article :/ (DocumentSlug $ Article.slug a)) <$> articleDyn)
      $ do
        el "h1" $ dynText $ Article.title <$> articleDyn
        el "p" $ dynText $ Article.description <$> articleDyn
        el "span" $ text "Read more..."

profileRoute
  :: Profile.Profile
  -> (R FrontendRoute)
profileRoute p = FrontendRoute_Profile :/ (Username (Profile.username p), Nothing)

profileImage
  :: ( DomBuilder t m
     , PostBuild t m
     )
  => Text -- Class
  -> Dynamic t (Maybe Text)
  -> m ()
profileImage className imageDyn =
  elDynAttr "img"
    ((\i -> Map.fromList [("src",imgUrl i),("class",className)]) <$> imageDyn)
    blank
