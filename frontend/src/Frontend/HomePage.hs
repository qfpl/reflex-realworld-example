{-# LANGUAGE FlexibleContexts, LambdaCase, MultiParamTypeClasses, OverloadedStrings, PatternSynonyms #-}
{-# LANGUAGE RecursiveDo, ScopedTypeVariables                                                        #-}
module Frontend.HomePage where

import Control.Lens    hiding (element)
import Reflex.Dom.Core

import Control.Monad.Fix      (MonadFix)
import Data.Foldable          (toList, traverse_)
import Data.Functor           (void)
import Data.Proxy             (Proxy (Proxy))
import Obelisk.Route          (R)
import Obelisk.Route.Frontend (RouteToUrl, SetRoute)
import Servant.Common.Req     (QParam (QNone))

import           Common.Conduit.Api.Articles.Articles (Articles (..))
import           Common.Conduit.Api.Namespace         (Namespace (Namespace), unNamespace)
import           Common.Route                         (FrontendRoute (..))
import           Frontend.ArticlePreview              (articlesPreview)
import qualified Frontend.Conduit.Client              as Client
import           Frontend.FrontendStateT
import           Frontend.Utils                       (buttonClass, buttonDynClass)

homePage
  :: forall t m s js
  . ( PostBuild t m
     , DomBuilder t m
     , RouteToUrl (R FrontendRoute) m
     , SetRoute t (R FrontendRoute) m
     , MonadHold t m
     , MonadFix m
     , HasLoggedInAccount s
     , HasFrontendState t s m
     , Prerender js t m
     )
  => m ()
homePage = elClass "div" "home-page" $ mdo
  tokDyn <- reviewFrontendState loggedInToken
  pbE <- getPostBuild

  -- TODO: This changes to an ADT with Feed/Global/Tag Text. Default changes by tokenDyn
  selectedTagDyn <- holdDyn Nothing $ leftmost
    [ (fmap Just . switchDyn . fmap leftmost $ tagSelectEDyn)
    , Nothing <$ globalSelectE
    ]

  (loadArtsE,_,_) <- Client.listArticles
    tokDyn
    (constDyn QNone)
    (constDyn QNone)
    (constDyn [])
    (constDyn [])
    (toList <$> selectedTagDyn)
    (leftmost [pbE,void $ updated selectedTagDyn, void $ updated tokDyn])

  -- Why doesn't postBuild work here?
  (loadTagsE,_,_) <- Client.allTags (leftmost [pbE,void $ updated tokDyn])

  artsDyn <- holdDyn (Articles [] 0) loadArtsE
  tagsDyn <- holdDyn (Namespace []) loadTagsE

  elClass "div" "banner" $
    elClass "div" "container" $ do
      elClass "h1" "logo-font" $ text "conduit"
      el "p" $ text "A place to share your knowledge"

  (globalSelectE, tagSelectEDyn) <- elClass "div" "container page" $ elClass "div" "row" $ do
    globalSelectE <- elClass "div" "col-md-9" $ do
      globalSelectE <- elClass "div" "feed-toggle" $
        elClass "ul" "nav nav-pills outline-active" $ do
          -- TODO Only show if logged in
          -- elClass "li" "nav-item" $ routeLinkClass "nav-link disabled" (FrontendRoute_Home :/ ()) $ text "Your Feed"
          let homeClassDyn = ("nav-link" <>) . maybe " active" (const "") <$> selectedTagDyn
          globalSelectE' <- elClass "li" "nav-item" $ buttonDynClass homeClassDyn (constDyn False) $ text "Global Feed"
          -- TODO Show a thing if we have a selected tag
          selectTagEE <- dyn $ ffor selectedTagDyn $ maybe (pure never) $ \t ->
            elClass "li" "nav-item" $ buttonClass "nav-link active" (constDyn False) $ text $ "#" <> t
          switchHold never selectTagEE

          pure globalSelectE'
      articlesPreview artsDyn
      pure globalSelectE

    tagSelectEDyn <- elClass "div" "col-md-3" $
      elClass "div" "sidebar" $ do
        el "p" $ text "Popular Tags"
        elClass "div" "tag-list" $ do
          simpleList (unNamespace <$> tagsDyn) tagPill
    pure (globalSelectE, tagSelectEDyn)
  pure ()
  where
    tagPill tDyn = do
      let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
            & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (\_ -> preventDefault)
            & elementConfig_initialAttributes .~ ("class" =: "tag-pill tag-default" <> "href" =: "")
      (e, _) <- element "a" cfg $ dynText tDyn
      pure $ current tDyn <@ domEvent Click e
