{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
module Frontend.FrontendStateT where

import           Control.Lens
import           Reflex.Dom.Core

import           Control.Monad.Fix                   (MonadFix)
import           Control.Monad.IO.Class              (MonadIO)
import           Control.Monad.Primitive             (PrimMonad (PrimState),
                                                      primitive)
import           Control.Monad.Ref                   (MonadRef (Ref), newRef,
                                                      readRef, writeRef)
import           Control.Monad.Trans                 (MonadTrans, lift)
import           Control.Monad.Trans.Control         (MonadTransControl (StT),
                                                      defaultLiftWith,
                                                      defaultRestoreT, liftWith,
                                                      restoreT)
import           Control.Monad.Trans.Reader          (ReaderT, runReaderT)
import           Data.Coerce                         (coerce)
import           Data.Constraint                     (Dict (..))
import           Data.Monoid                         (Endo (Endo))
import           Language.Javascript.JSaddle         (MonadJSM)
import           Obelisk.Route.Frontend              (RouteToUrl, SetRoute,
                                                      askRouteToUrl,
                                                      modifyRoute, setRoute)
import           Reflex.Host.Class                   (MonadReflexCreateTrigger)

import           RealWorld.Conduit.Api.Users.Account (Account)

data FrontendEvent = LogOut | LogIn Account

data FrontendState = FrontendState
  { _frontendStateLoggedInAccount :: Maybe Account
  }
makeLenses ''FrontendState

initialFrontendState :: FrontendState
initialFrontendState = FrontendState Nothing

updateFrontendState :: FrontendEvent -> Endo FrontendState
updateFrontendState e = Endo $ case e of
  LogOut  -> frontendStateLoggedInAccount .~ Nothing
  LogIn a -> frontendStateLoggedInAccount .~ Just a

newtype FrontendStateT t m a = FrontendStateT
  { unFrontendStateT :: ReaderT (Dynamic t FrontendState) m a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadTrans, NotReady t,
            MonadHold t, MonadSample t, PostBuild t, TriggerEvent t, MonadIO,
            MonadReflexCreateTrigger t, HasDocument)

runFrontendStateT :: FrontendStateT t m a -> Dynamic t FrontendState -> m a
runFrontendStateT t = runReaderT (unFrontendStateT t)

instance (Monad m, RouteToUrl r m) => RouteToUrl r (FrontendStateT t m) where
  askRouteToUrl = lift askRouteToUrl

instance (Monad m, SetRoute t r m) => SetRoute t r (FrontendStateT t m) where
  setRoute = lift . setRoute
  modifyRoute = lift . modifyRoute

instance HasJSContext m => HasJSContext (FrontendStateT t m) where
  type JSContextPhantom (FrontendStateT t m) = JSContextPhantom m
  askJSContext = lift askJSContext

instance Prerender js m => Prerender js (FrontendStateT t m) where
  prerenderClientDict = fmap (\Dict -> Dict) (prerenderClientDict :: Maybe (Dict (PrerenderClientConstraint js m)))

instance Requester t m => Requester t (FrontendStateT t m) where
  type Request (FrontendStateT t m) = Request m
  type Response (FrontendStateT t m) = Response m
  requesting = FrontendStateT . requesting
  requesting_ = FrontendStateT . requesting_

#ifndef ghcjs_HOST_OS
deriving instance MonadJSM m => MonadJSM (FrontendStateT t m)
#endif

instance PerformEvent t m => PerformEvent t (FrontendStateT t m) where
  type Performable (FrontendStateT t m) = Performable m
  performEvent = lift . performEvent
  performEvent_ = lift . performEvent_

instance MonadRef m => MonadRef (FrontendStateT t m) where
  type Ref (FrontendStateT t m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

instance HasJS x m => HasJS x (FrontendStateT t m) where
  type JSX (FrontendStateT t m) = JSX m
  liftJS = lift . liftJS

instance MonadTransControl (FrontendStateT t) where
  type StT (FrontendStateT t) a = StT (ReaderT (Dynamic t FrontendState)) a
  liftWith = defaultLiftWith FrontendStateT unFrontendStateT
  restoreT = defaultRestoreT FrontendStateT

instance PrimMonad m => PrimMonad (FrontendStateT t m ) where
  type PrimState (FrontendStateT t m) = PrimState m
  primitive = lift . primitive

instance DomBuilder t m => DomBuilder t (FrontendStateT t m) where
  type DomBuilderSpace (FrontendStateT t m) = DomBuilderSpace m

instance Adjustable t m => Adjustable t (FrontendStateT t m) where
  runWithReplace a0 a' = FrontendStateT $ runWithReplace (coerce a0) $ coerceEvent a'
  traverseIntMapWithKeyWithAdjust f a0 a' = FrontendStateT $ traverseIntMapWithKeyWithAdjust (coerce f) (coerce a0) $ coerce a'
  traverseDMapWithKeyWithAdjust f a0 a' = FrontendStateT $ traverseDMapWithKeyWithAdjust (\k v -> coerce $ f k v) (coerce a0) $ coerce a'
  traverseDMapWithKeyWithAdjustWithMove f a0 a' = FrontendStateT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> coerce $ f k v) (coerce a0) $ coerce a'

instance (Monad m, MonadQuery t vs m) => MonadQuery t vs (FrontendStateT t m) where
  tellQueryIncremental = lift . tellQueryIncremental
  askQueryResult = lift askQueryResult
  queryIncremental = lift . queryIncremental
