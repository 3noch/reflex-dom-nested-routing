{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Reflex.Dom.NestedRoute where

import           Control.Lens                 (Rewrapped, Wrapped(..), iso, to, (^.))
import           Control.Monad.Exception      (MonadAsyncException, MonadException)
import           Control.Monad.Fix
import           Control.Monad.Primitive      (PrimMonad, PrimState, primitive)
import           Control.Monad.Reader
import           Control.Monad.Ref
import           Control.Monad.State
import qualified Control.Monad.State.Strict   as StrictState
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Char8        as B8
import           Data.Coerce                  (coerce)
import           Data.Functor                 ((<$))
import qualified Data.List                    as List
import           Data.Maybe                   (listToMaybe)
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Text.Encoding           (decodeUtf8, encodeUtf8)
import           Foreign.JavaScript.TH
import           Language.Javascript.JSaddle  (MonadJSM)
import           Reflex
import           Reflex.Dom.Builder.Class
import           Reflex.Dom.Builder.Immediate
import           Reflex.Dom.Contrib.Router    (route')
import           Reflex.Dom.Core
import           Reflex.Host.Class
import           URI.ByteString               (URIRef, fragmentL, pathL)


data RouteContext segment t = RouteContext
  { _routeContext_allSegments    :: Dynamic t [segment]
  , _routeContext_nextSegments   :: Dynamic t [segment]
  , _routeContext_currentSegment :: Dynamic t (Maybe segment)
  }


class (Reflex t, Monad m) => HasRoute segment t m | m -> segment, m -> t where
  routeContext :: m (RouteContext segment t)
  withSegments :: (RouteContext segment t -> RouteContext segment t) -> m a -> m a


newtype RouteT segment t m a = RouteT { unRouteT :: ReaderT (RouteContext segment t) m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadHold t,
            MonadSample t, MonadAsyncException, MonadException, MonadTrans, PostBuild t,
            MonadReflexCreateTrigger t, TriggerEvent t, MonadAtomicRef)

instance (MonadWidget t m) => HasRoute segment t (RouteT segment t m) where
  routeContext = RouteT ask
  withSegments f (RouteT m) = RouteT $ local f m

instance Requester t m => Requester t (RouteT segment t m) where
  type Request (RouteT segment t m) = Request m
  type Response (RouteT segment t m) = Response m
  requesting = lift . requesting
  requesting_ = lift . requesting_

instance HasRoute segment t m => HasRoute segment t (RequesterT t request response m) where
  routeContext = lift routeContext
  withSegments f (RequesterT a) = RequesterT $ StrictState.mapStateT (mapReaderT $ withSegments f) a

instance Wrapped (RouteT segment t m a) where
  type Unwrapped (RouteT segment t m a) = ReaderT (RouteContext segment t) m a
  _Wrapped' = iso coerce coerce

instance RouteT segment t m a ~ x => Rewrapped (RouteT segment t m a) x

instance PerformEvent t m => PerformEvent t (RouteT segment t m) where
 type Performable (RouteT segment t m) = Performable m
 {-# INLINABLE performEvent_ #-}
 performEvent_ = lift . performEvent_
 {-# INLINABLE performEvent #-}
 performEvent = lift . performEvent

instance MonadRef m => MonadRef (RouteT segment t m) where
  type Ref (RouteT segment t m) = Ref m
  {-# INLINABLE newRef #-}
  newRef = lift . newRef
  {-# INLINABLE readRef #-}
  readRef = lift . readRef
  {-# INLINABLE writeRef #-}
  writeRef r = lift . writeRef r

instance (MonadAdjust t m, MonadHold t m) => MonadAdjust t (RouteT segment t m) where
  runWithReplace a0 a' = RouteT $ runWithReplace (unRouteT a0) (fmapCheap unRouteT a')
  traverseDMapWithKeyWithAdjust f dm edm = RouteT $ traverseDMapWithKeyWithAdjust (\k v -> unRouteT $ f k v) (coerce dm) (coerceEvent edm)
  {-# INLINABLE traverseDMapWithKeyWithAdjust #-}
  traverseDMapWithKeyWithAdjustWithMove f dm edm = RouteT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> unRouteT $ f k v) (coerce dm) (coerceEvent edm)

instance (DomBuilder t m, MonadHold t m, MonadFix m) => DomBuilder t (RouteT segment t m) where
  type DomBuilderSpace (RouteT segment t m) = DomBuilderSpace m
  textNode = lift . textNode
  element elementTag cfg (RouteT child) = RouteT $ element elementTag cfg child
  inputElement = lift . inputElement
  textAreaElement = lift . textAreaElement
  selectElement cfg (RouteT child) = RouteT $ selectElement cfg child
  placeRawElement = lift . placeRawElement
  wrapRawElement e = lift . wrapRawElement e

instance MonadReader r m => MonadReader r (RouteT segment t m) where
  ask = lift ask
  local f (RouteT a) = RouteT $ mapReaderT (local f) a

instance MonadState s m => MonadState s (RouteT segment t m) where
  get = lift get
  put s = lift $ put s

instance EventWriter t w m => EventWriter t w (RouteT segment t m) where
  tellEvent = lift . tellEvent


instance HasDocument m => HasDocument (RouteT segment t m)
instance HasJSContext m => HasJSContext (RouteT segment t m) where
  type JSContextPhantom (RouteT segment t m) = JSContextPhantom m
  askJSContext = RouteT askJSContext
#ifndef ghcjs_HOST_OS
instance MonadJSM m => MonadJSM (RouteT segment t m)
#endif


instance PrimMonad m => PrimMonad (RouteT segment t m) where
  type PrimState (RouteT segment t m) = PrimState m
  primitive = lift . primitive

--instance HasMountStatus t m => HasMountStatus t (RouteT segment r m) where
--  getMountStatus = lift getMountStatus


runRoute :: forall segment t m. (MonadWidget t m, Eq segment)
         => (forall a. URIRef a -> [segment])
         -> (forall a. URIRef a -> [segment] -> URIRef a)
         -> RouteT segment t m (Event t [segment])
         -> m ()
runRoute toSegments fromSegments (RouteT f) = do
  let
    routeHandler = route' (\_ uri -> uri) id
  rec
    dynamicRoute <- routeHandler routeChanged
    routeChanged <- pathToHandler dynamicRoute
  pure ()

  where
    pathToHandler :: Dynamic t (URIRef a) -> m (Event t (URIRef a))
    pathToHandler uri = do
      let
        allSegments = toSegments <$> uri
        ctx = RouteContext{ _routeContext_allSegments    = allSegments
                          , _routeContext_nextSegments   = allSegments
                          , _routeContext_currentSegment = pure Nothing
                          }
      newSegments <- runReaderT f ctx
      let x = ffor uri $ \uri' -> fromSegments uri' <$> newSegments
      pure (switch (current x))


runRouteWithPathInFragment
  :: forall t m. (MonadWidget t m)
  => RouteT Text t m (Event t [Text])
  -> m ()
runRouteWithPathInFragment = runRoute
  (T.splitOn "/" . T.dropAround (=='/') . fragAsText)
  (\oldUrl -> setFrag oldUrl . T.intercalate "/")


withRoute
  :: forall a segment t m. (DomBuilder t m, MonadFix m, PostBuild t m, MonadHold t m, HasRoute segment t m, Eq segment)
  => (Maybe segment -> m a)
  -- ^ A routing function that produces a widget from a segment.
  -> m (Event t a)
withRoute f = do
  ctx <- routeContext
  let segmentsFlat = List.uncons <$> _routeContext_nextSegments ctx
  segmentsNested <- maybeDyn segmentsFlat

  let
    component = ffor segmentsNested $ \x -> case x of
      Nothing            -> do
        let
          segment = Nothing
          newCtx  = ctx{ _routeContext_currentSegment = pure segment
                       , _routeContext_nextSegments   = pure []
                       }

        a <- withSegments (const newCtx) (f segment)
        postBuildEv <- getPostBuild
        pure (a <$ postBuildEv)  -- Wrap the value up in an Event to unify both paths.

      Just segmentUncons -> do
        segmentDyn <- fmap Just <$> holdUniqDyn (fst <$> segmentUncons)
        let newCtx = ctx{ _routeContext_currentSegment = segmentDyn
                        , _routeContext_nextSegments   = snd <$> segmentUncons
                        }

        dyn $ ffor segmentDyn $ \segment ->
          withSegments (const newCtx) (f segment)

  switchPromptly never =<< dyn component


allRouteSegments :: (Reflex t, MonadHold t m, MonadFix m, HasRoute segment t m, Eq segment) => m (Dynamic t [segment])
allRouteSegments = _routeContext_allSegments <$> routeContext

currentRouteSegment :: (Functor m, HasRoute segment t m) => m (Dynamic t (Maybe segment))
currentRouteSegment = _routeContext_currentSegment <$> routeContext

nextRouteSegment :: (Reflex t, MonadHold t m, MonadFix m, HasRoute segment t m, Eq segment) => m (Dynamic t (Maybe segment))
nextRouteSegment = do
  ctx <- routeContext
  holdUniqDyn (listToMaybe <$> _routeContext_nextSegments ctx)

redirectLocally :: (MonadWidget t m, HasRoute segment t m) => [segment] -> m (Event t [segment])
redirectLocally segments = (segments <$) <$> getPostBuild


setFrag :: URIRef a -> Text -> URIRef a
setFrag uri p = uri & fragmentL .~ (Just $ encodeUtf8 p)

fragAsText :: URIRef a -> Text
fragAsText uri = maybe "" decodeUtf8 (uri ^. fragmentL)

pathSegments :: URIRef a -> [ByteString]
pathSegments uri =  uri ^. pathL . to (B8.split '/')

