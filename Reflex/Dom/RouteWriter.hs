{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Reflex.Dom.RouteWriter where

import           Control.Lens                 (Rewrapped, Wrapped (..), iso)
import           Control.Monad.Exception      (MonadAsyncException,
                                               MonadException)
import           Control.Monad.Fix
import           Control.Monad.Primitive      (PrimMonad, PrimState, primitive)
import           Control.Monad.Reader
import           Control.Monad.Ref
import qualified Control.Monad.State.Strict   as StrictState
import           Data.Coerce                  (coerce)
import           Foreign.JavaScript.TH
import           Language.Javascript.JSaddle  (MonadJSM)
import           Reflex
import           Reflex.Dom.Builder.Class
import           Reflex.Dom.Builder.Immediate
import           Reflex.Dom.Core
import           Reflex.Host.Class

import           Reflex.Dom.NestedRoute


class (Reflex t, Monad m) => RouteWriter t segment m | m -> segment, m -> t where
  tellRoute :: Event t [segment] -> m ()

newtype RouteWriterT t segment m a = RouteWriterT { unRouteWriterT :: EventWriterT t [segment] m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadHold t,
            MonadSample t, MonadAsyncException, MonadException, MonadTrans, PostBuild t,
            MonadReflexCreateTrigger t, TriggerEvent t, MonadAtomicRef)


instance (MonadWidget t m) => RouteWriter t segment (RouteWriterT t segment m) where
  tellRoute = RouteWriterT . tellEvent


instance Requester t m => Requester t (RouteWriterT t segment m) where
  type Request (RouteWriterT t segment m) = Request m
  type Response (RouteWriterT t segment m) = Response m
  requesting = lift . requesting
  requesting_ = lift . requesting_

instance RouteWriter t segment m => RouteWriter t segment (RequesterT t request response m) where
  tellRoute = lift . tellRoute

instance Wrapped (RouteWriterT t segment m a) where
  type Unwrapped (RouteWriterT t segment m a) = EventWriterT t [segment] m a
  _Wrapped' = iso coerce coerce

instance RouteWriterT t segment m a ~ x => Rewrapped (RouteWriterT t segment m a) x

instance PerformEvent t m => PerformEvent t (RouteWriterT t segment m) where
  type Performable (RouteWriterT t segment m) = Performable m
  {-# INLINABLE performEvent_ #-}
  performEvent_ = lift . performEvent_
  {-# INLINABLE performEvent #-}
  performEvent = lift . performEvent

instance MonadRef m => MonadRef (RouteWriterT t segment m) where
  type Ref (RouteWriterT t segment m) = Ref m
  {-# INLINABLE newRef #-}
  newRef = lift . newRef
  {-# INLINABLE readRef #-}
  readRef = lift . readRef
  {-# INLINABLE writeRef #-}
  writeRef r = lift . writeRef r

instance (Adjustable t m, MonadHold t m) => Adjustable t (RouteWriterT t segment m) where
  runWithReplace a0 a' = RouteWriterT $ runWithReplace (unRouteWriterT a0) (fmapCheap unRouteWriterT a')
  traverseDMapWithKeyWithAdjust f dm edm = RouteWriterT $ traverseDMapWithKeyWithAdjust (\k v -> unRouteWriterT $ f k v) (coerce dm) (coerceEvent edm)
  {-# INLINABLE traverseDMapWithKeyWithAdjust #-}
  traverseDMapWithKeyWithAdjustWithMove f dm edm = RouteWriterT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> unRouteWriterT $ f k v) (coerce dm) (coerceEvent edm)

instance (DomBuilder t m, MonadHold t m, MonadFix m) => DomBuilder t (RouteWriterT t segment m) where
  type DomBuilderSpace (RouteWriterT t segment m) = DomBuilderSpace m
  textNode = lift . textNode
  element elementTag cfg (RouteWriterT child) = RouteWriterT $ element elementTag cfg child
  inputElement = lift . inputElement
  textAreaElement = lift . textAreaElement
  selectElement cfg (RouteWriterT child) = RouteWriterT $ selectElement cfg child
  placeRawElement = lift . placeRawElement
  wrapRawElement e = lift . wrapRawElement e

deriving instance MonadReader r m => MonadReader r (RouteWriterT t segment m)

instance RouteWriter t segment m => RouteWriter t segment (EventWriterT t w m) where
  tellRoute = lift . tellRoute
instance EventWriter t w m => EventWriter t w (RouteWriterT t segment m) where
  tellEvent = lift . tellEvent

instance RouteWriter t segment m => RouteWriter t segment (DynamicWriterT t w m) where
  tellRoute = lift . tellRoute
instance MonadDynamicWriter t w m => MonadDynamicWriter t w (RouteWriterT t segment m) where
  tellDyn = lift . tellDyn


instance HasDocument m => HasDocument (RouteWriterT t segment m)
instance HasJSContext m => HasJSContext (RouteWriterT t segment m) where
  type JSContextPhantom (RouteWriterT t segment m) = JSContextPhantom m
  askJSContext = RouteWriterT askJSContext
#ifndef ghcjs_HOST_OS
instance MonadJSM m => MonadJSM (RouteWriterT segment t m)
#endif


instance PrimMonad m => PrimMonad (RouteWriterT t segment m) where
  type PrimState (RouteWriterT t segment m) = PrimState m
  primitive = lift . primitive

deriving instance HasRoute t segment m => HasRoute t segment (RouteWriterT t segment m)

--instance HasMountStatus t m => HasMountStatus t (RouteWriterT r segment m) where
--  getMountStatus = lift getMountStatus

-- | Runs an action that can easily set new routes at will anywhere in the action.
runRouteWriterT :: (Monad m, Reflex t) => RouteWriterT t segment m a -> m (a, Event t [segment])
runRouteWriterT (RouteWriterT m) = runEventWriterT m

-- | Sets a new route immediately (on post-build) to the given route.
tellRedirectLocally :: (PostBuild t m, RouteWriter t segment m) => [segment] -> m ()
tellRedirectLocally segments = tellRoute =<< redirectLocally segments

-- | Alias for 'tellRedirectLocally'
localRedirect :: (PostBuild t m, RouteWriter t segment m) => [segment] -> m ()
localRedirect = tellRedirectLocally


-- | Sets a new route to the given route whenever the given 'Event' fires, ignoring the 'Event' payload.
tellRouteAs :: (RouteWriter t segment m) => [segment] -> Event t a -> m ()
tellRouteAs segments ev = tellRoute (segments <$ ev)

-- | Sets a new route by transforming an arbitrary 'Event' payload into a route.
tellRouteBy :: (RouteWriter t segment m) => (a -> [segment]) -> Event t a -> m ()
tellRouteBy toSegments ev = tellRoute (toSegments <$> ev)

-- | Changes the route with an arbitrary function over the current route.
tellRouteModifyWith
  :: (RouteWriter t segment m, HasRoute t segment m, MonadHold t m, MonadFix m, Eq segment)
  => Event t ([segment] -> [segment]) -> m ()
tellRouteModifyWith ev = do
  segments <- allRouteSegments
  tellRoute $ attachWith (&) (current segments) ev

-- | Sets a new route relative to the current layer in the routing hierarchy.
tellRouteRelative
  :: (RouteWriter t segment m, HasRoute t segment m, MonadHold t m, MonadFix m, Eq segment)
  => Event t [segment] -> m ()
tellRouteRelative ev = do
  parents <- parentRouteSegments
  tellRoute $ attachWith (++) (current parents) ev

-- Like 'tellRouteAs' and 'tellRouteRelative'.
tellRouteRelativeAs
  :: (RouteWriter t segment m, HasRoute t segment m, MonadHold t m, MonadFix m, Eq segment)
  => [segment] -> Event t a -> m ()
tellRouteRelativeAs segments ev = tellRouteRelative (segments <$ ev)
