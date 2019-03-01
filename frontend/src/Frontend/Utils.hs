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
