{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}
module Frontend.Utils where

import           Control.Lens           hiding (element)
import           Reflex.Dom.Core

import qualified Data.Map               as Map
import           Data.Proxy             (Proxy (Proxy))
import           Data.Text              (Text)
import           Obelisk.Route.Frontend (RouteToUrl, SetRoute, askRouteToUrl,
                                         setRoute)

-- These should probably be in obelisk!
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
routeLinkClass = routeLinkDynClass . constDyn

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
routeLinkAttr = routeLinkDynAttr . constDyn

routeLinkDynClass
  :: forall t m a r
  .  ( DomBuilder t m
     , RouteToUrl r m
     , SetRoute t r m
     , PostBuild t m
     , MonadSample t m
     )
  => Dynamic t Text
  -> r
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
  -> r
  -> m a
  -> m a
routeLinkDynAttr attrDyn r m = do
  enc <- askRouteToUrl
  let addHref = Map.insert "href" (enc r)
  initAttr <- fmap addHref <$> sample . current $ attrDyn
  modAttrs <- dynamicAttributesToModifyAttributesWithInitial initAttr (addHref <$> attrDyn)
  let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (\_ -> preventDefault)
        & elementConfig_initialAttributes .~ initAttr
        & elementConfig_modifyAttributes  .~ modAttrs
  (e, a) <- element "a" cfg m
  setRoute $ r <$ domEvent Click e
  return a
