{-# LANGUAGE FlexibleContexts, MultiParamTypeClasses, OverloadedStrings, ScopedTypeVariables #-}
module Frontend.Utils where

import Control.Lens    hiding (element)
import Reflex.Dom.Core

import           Control.Monad          (mfilter)
import           Control.Monad.Trans    (lift)
import           Data.Bool              (bool)
import qualified Data.Map               as Map
import           Data.Maybe             (fromMaybe)
import           Data.Proxy             (Proxy (Proxy))
import           Data.Text              (Text)
import qualified Data.Text              as T
import           Obelisk.Route.Frontend (RouteToUrl, RoutedT, SetRoute, askRoute, askRouteToUrl, runRoutedT,
                                         setRoute)

showText :: Show s => s -> Text
showText = T.pack . show

imgUrl :: Maybe Text -> Text
imgUrl =
  fromMaybe "https://static.productionready.io/images/smiley-cyrus.jpg"
  . mfilter (not . T.null . T.strip)

-- These should probably be in obelisk!

pathSegmentSubRoute :: (Monad m, Functor (Dynamic t)) => (Dynamic t a -> RoutedT t b m c) -> RoutedT t (a, b) m c
pathSegmentSubRoute f = do
  rDyn <- askRoute
  lift $ runRoutedT (f (Prelude.fst <$> rDyn)) (Prelude.snd <$> rDyn)

buttonClass
  :: forall t m a
  . (DomBuilder t m, PostBuild t m)
  => Text
  -> Dynamic t Bool
  -> m a
  -> m (Event t ())
buttonClass cls = buttonDynClass (constDyn cls)

buttonDynClass
  :: forall t m a
  . (DomBuilder t m, PostBuild t m)
  => Dynamic t Text
  -> Dynamic t Bool
  -> m a
  -> m (Event t ())
buttonDynClass clsDyn disabledDyn m = do
  let attrsDyn = (<>) <$> (("class" =:) <$> clsDyn) <*> (bool (Map.empty) ("disabled" =: "") <$> disabledDyn)
  modAttrs <- dynamicAttributesToModifyAttributes attrsDyn
  let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (\_ -> preventDefault)
        & elementConfig_modifyAttributes .~ modAttrs
  (e, _) <- element "button" cfg m
  pure $ domEvent Click e

routeLinkClass
  :: forall t m a r
  .  ( DomBuilder t m
     , RouteToUrl r m
     , SetRoute t r m
     , PostBuild t m
     , MonadSample t m
     )
  => Text
  -> r
  -> m a
  -> m a
routeLinkClass c = routeLinkDynClass (constDyn c) . constDyn

routeLinkAttr
  :: forall t m a r
  .  ( DomBuilder t m
     , RouteToUrl r m
     , SetRoute t r m
     , PostBuild t m
     , MonadSample t m
     )
  => Map.Map AttributeName Text
  -> r
  -> m a
  -> m a
routeLinkAttr attrs = routeLinkDynAttr (constDyn attrs) . constDyn

routeLinkDyn
  :: forall t m a r
  .  ( DomBuilder t m
     , RouteToUrl r m
     , SetRoute t r m
     , PostBuild t m
     , MonadSample t m
     )
  => Dynamic t r
  -> m a
  -> m a
routeLinkDyn = routeLinkDynAttr (constDyn Map.empty)

routeLinkDynClass
  :: forall t m a r
  .  ( DomBuilder t m
     , RouteToUrl r m
     , SetRoute t r m
     , PostBuild t m
     , MonadSample t m
     )
  => Dynamic t Text
  -> Dynamic t r
  -> m a
  -> m a
routeLinkDynClass cDyn = routeLinkDynAttr (("class" =:) <$> cDyn)

routeLinkDynAttr
  :: forall t m a r
  .  ( DomBuilder t m
     , RouteToUrl r m
     , SetRoute t r m
     , PostBuild t m
     , MonadSample t m
     )
  => Dynamic t (Map.Map AttributeName Text)
  -> Dynamic t r
  -> m a
  -> m a
routeLinkDynAttr attrDyn rDyn m = do
  enc <- askRouteToUrl
  let attrsDyn = (Map.insert "href" . enc <$> rDyn <*> attrDyn)
  initAttrs <- sample . current $ attrsDyn
  modAttrs <- dynamicAttributesToModifyAttributesWithInitial initAttrs attrsDyn
  let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (\_ -> preventDefault)
        & elementConfig_initialAttributes .~ initAttrs
        & elementConfig_modifyAttributes  .~ modAttrs
  (e, a) <- element "a" cfg m
  setRoute $ current rDyn <@ domEvent Click e
  return a
