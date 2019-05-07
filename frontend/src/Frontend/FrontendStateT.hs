{-# LANGUAGE CPP, FlexibleContexts, FlexibleInstances, FunctionalDependencies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase, MultiParamTypeClasses, PatternSynonyms, RankNTypes, ScopedTypeVariables          #-}
{-# LANGUAGE StandaloneDeriving, TemplateHaskell, TypeFamilies, UndecidableInstances                      #-}
module Frontend.FrontendStateT where

import Control.Lens
import Reflex.Dom.Core

import Control.Monad.Fix           (MonadFix)
import Control.Monad.IO.Class      (MonadIO)
import Control.Monad.Primitive     (PrimMonad (PrimState), primitive)
import Control.Monad.Ref           (MonadRef (Ref), newRef, readRef, writeRef)
import Control.Monad.Trans         (MonadTrans, lift)
import Control.Monad.Trans.Control (MonadTransControl (StT), defaultLiftWith, defaultRestoreT, liftWith,
                                    restoreT)
import Control.Monad.Trans.Reader  (ReaderT, ask, runReaderT)
import Data.Coerce                 (coerce)
import Data.Functor                (void)
import Data.Monoid                 (Endo (Endo), First)
import Language.Javascript.JSaddle (MonadJSM)
import Obelisk.Route.Frontend      (pattern (:/), R, RouteToUrl, RoutedT, SetRoute, askRouteToUrl,
                                    modifyRoute, setRoute)
import Reflex.Dom.Storage.Base     (StorageT)
import Reflex.Host.Class           (MonadReflexCreateTrigger)

import           Common.Conduit.Api.User.Account (Account, Token)
import qualified Common.Conduit.Api.User.Account as Account
import           Common.Route                    (FrontendRoute (FrontendRoute_Home))


data FrontendEvent = LogOut | LogIn Account
makeClassyPrisms ''FrontendEvent

data LoadableData e a = Loading | LoadFailed e | Loaded a
makePrisms ''LoadableData

loadableData :: m -> (e -> m) -> (a -> m) -> LoadableData e a -> m
loadableData loading loadFailed loaded = \case
  Loading      -> loading
  LoadFailed e -> loadFailed e
  Loaded     a -> loaded a

data FrontendData = FrontendData
  { _frontendDataLoggedInAccount :: LoadableData () (Maybe Account)
  }
makeClassy ''FrontendData

class HasLoggedInAccount s where
  loadableLoggedInAccount :: Getter s (LoadableData () (Maybe Account))

  loggedInAccount :: Fold s Account
  loggedInAccount = loadableLoggedInAccount . _Loaded . _Just

instance HasLoggedInAccount FrontendData where
  loadableLoggedInAccount = frontendDataLoggedInAccount

initialFrontendData :: FrontendData
initialFrontendData = FrontendData Loading

updateFrontendData :: FrontendEvent -> Endo FrontendData
updateFrontendData e = Endo $ case e of
  LogOut  -> frontendDataLoggedInAccount .~ Loaded Nothing
  LogIn a -> frontendDataLoggedInAccount .~ (Loaded $ Just a)

loggedInToken :: HasLoggedInAccount t => Fold t Token
loggedInToken = loggedInAccount . to Account.token

newtype FrontendStateT t s m a = FrontendStateT
  { unFrontendStateT :: ReaderT (Dynamic t s) m a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadTrans, NotReady t,
            MonadHold t, MonadSample t, PostBuild t, TriggerEvent t, MonadIO,
            MonadReflexCreateTrigger t, HasDocument, DomRenderHook t)

runFrontendStateT :: FrontendStateT t s m a -> Dynamic t s -> m a
runFrontendStateT t = runReaderT (unFrontendStateT t)

class HasFrontendState t s m | m -> s where
  askFrontendState :: m (Dynamic t s)

viewFrontendState
  :: (HasFrontendState t s m, Functor (Dynamic t), Functor m)
  => Getter s a
  -> m (Dynamic t a)
viewFrontendState g = fmap (^. g) <$> askFrontendState

reviewFrontendState
  :: (HasFrontendState t s m, Functor (Dynamic t), Monad m)
  => Getting (First a) s a
  -> m (Dynamic t (Maybe a))
reviewFrontendState g = fmap (^? g) <$> askFrontendState

noUserWidget
  :: ( HasLoggedInAccount s
     , HasFrontendState t s m
     , SetRoute t (R FrontendRoute) m
     , DomBuilder t m
     , PostBuild t m
     )
  => m ()
  -> m ()
noUserWidget w = withUser w (const $ redirect (FrontendRoute_Home :/ ()))

userWidget
  :: ( HasLoggedInAccount s
     , HasFrontendState t s m
     , SetRoute t (R FrontendRoute) m
     , DomBuilder t m
     , PostBuild t m
     )
  => (Account -> m ())
  -> m ()
userWidget = withUser (redirect (FrontendRoute_Home :/ ()))

withUser
  :: ( HasLoggedInAccount s
     , HasFrontendState t s m
     , Monad m
     , DomBuilder t m
     , PostBuild t m
     )
  => m ()
  -> (Account -> m ())
  -> m ()
withUser noUserW userW = do
  loadingAccountDyn <- viewFrontendState loadableLoggedInAccount
  void . dyn . ffor loadingAccountDyn $ loadableData blank (const blank) (maybe noUserW userW)

redirect :: ( SetRoute t r m , PostBuild t m) => r -> m ()
redirect r = do
  pbE <- getPostBuild
  setRoute $ r <$ pbE

instance Monad m => HasFrontendState t s (FrontendStateT t s m) where
  askFrontendState = FrontendStateT ask

instance (Monad m, HasFrontendState t s m) => HasFrontendState t s (RoutedT t r m) where
  askFrontendState = lift askFrontendState

instance (Monad m, HasFrontendState t s m) => HasFrontendState t s (EventWriterT t w m) where
  askFrontendState = lift askFrontendState

instance (Monad m, HasFrontendState t s m) => HasFrontendState t s (StorageT t k m) where
  askFrontendState = lift askFrontendState

instance (Monad m, RouteToUrl r m) => RouteToUrl r (FrontendStateT t s m) where
  askRouteToUrl = lift askRouteToUrl

instance (Monad m, SetRoute t r m) => SetRoute t r (FrontendStateT t s m) where
  setRoute = lift . setRoute
  modifyRoute = lift . modifyRoute

instance HasJSContext m => HasJSContext (FrontendStateT t s m) where
  type JSContextPhantom (FrontendStateT t s m) = JSContextPhantom m
  askJSContext = lift askJSContext

instance (Prerender js t m, Monad m) => Prerender js t (FrontendStateT t s m) where
  type Client (FrontendStateT t s m) = FrontendStateT t s (Client m)
  prerender server client = FrontendStateT $ do
    env <- ask
    lift $ prerender (runReaderT (unFrontendStateT server) env) (runReaderT (unFrontendStateT client) env)

instance Requester t m => Requester t (FrontendStateT t s m) where
  type Request (FrontendStateT t s m) = Request m
  type Response (FrontendStateT t s m) = Response m
  requesting = FrontendStateT . requesting
  requesting_ = FrontendStateT . requesting_

#ifndef ghcjs_HOST_OS
deriving instance MonadJSM m => MonadJSM (FrontendStateT t s m)
#endif

instance PerformEvent t m => PerformEvent t (FrontendStateT t s m) where
  type Performable (FrontendStateT t s m) = Performable m
  performEvent = lift . performEvent
  performEvent_ = lift . performEvent_

instance MonadRef m => MonadRef (FrontendStateT t s m) where
  type Ref (FrontendStateT t s m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

instance HasJS x m => HasJS x (FrontendStateT t s m) where
  type JSX (FrontendStateT t s m) = JSX m
  liftJS = lift . liftJS

instance MonadTransControl (FrontendStateT t s) where
  type StT (FrontendStateT t s) a = StT (ReaderT (Dynamic t s)) a
  liftWith = defaultLiftWith FrontendStateT unFrontendStateT
  restoreT = defaultRestoreT FrontendStateT

instance PrimMonad m => PrimMonad (FrontendStateT t s m ) where
  type PrimState (FrontendStateT t s m) = PrimState m
  primitive = lift . primitive

instance DomBuilder t m => DomBuilder t (FrontendStateT t s m) where
  type DomBuilderSpace (FrontendStateT t s m) = DomBuilderSpace m

instance Adjustable t m => Adjustable t (FrontendStateT t s m) where
  runWithReplace a0 a' = FrontendStateT $ runWithReplace (coerce a0) $ coerceEvent a'
  traverseIntMapWithKeyWithAdjust f a0 a' = FrontendStateT $ traverseIntMapWithKeyWithAdjust (coerce f) (coerce a0) $ coerce a'
  traverseDMapWithKeyWithAdjust f a0 a' = FrontendStateT $ traverseDMapWithKeyWithAdjust (\k v -> coerce $ f k v) (coerce a0) $ coerce a'
  traverseDMapWithKeyWithAdjustWithMove f a0 a' = FrontendStateT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> coerce $ f k v) (coerce a0) $ coerce a'

instance (Monad m, MonadQuery t vs m) => MonadQuery t vs (FrontendStateT t s m) where
  tellQueryIncremental = lift . tellQueryIncremental
  askQueryResult = lift askQueryResult
  queryIncremental = lift . queryIncremental
