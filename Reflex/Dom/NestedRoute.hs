{-# LANGUAGE CPP                        #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

module Reflex.Dom.NestedRoute where

import           Control.Lens                 (Rewrapped, Wrapped (..), iso, to,
                                               (^.))
import           Control.Monad.Exception      (MonadAsyncException,
                                               MonadException)
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
  { _routeContext_allSegments    :: !(Dynamic t [segment])
  , _routeContext_nextSegments   :: !(Dynamic t [segment])
  , _routeContext_currentSegment :: !(Dynamic t (Maybe segment))
  , _routeContext_currentDepth   :: !Int
  }


class (Reflex t, Monad m) => HasRoute t segment m | m -> segment, m -> t where
  routeContext :: m (RouteContext segment t)
  withSegments :: (RouteContext segment t -> RouteContext segment t) -> m a -> m a


newtype RouteT t segment m a = RouteT { unRouteT :: ReaderT (RouteContext segment t) m a }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadHold t,
            MonadSample t, MonadAsyncException, MonadException, MonadTrans, PostBuild t,
            MonadReflexCreateTrigger t, TriggerEvent t, MonadAtomicRef)


instance (MonadWidget t m) => HasRoute t segment (RouteT t segment m) where
  routeContext = RouteT ask
  withSegments f (RouteT m) = RouteT $ local f m

instance Requester t m => Requester t (RouteT t segment m) where
  type Request (RouteT t segment m) = Request m
  type Response (RouteT t segment m) = Response m
  requesting = lift . requesting
  requesting_ = lift . requesting_

instance HasRoute t segment m => HasRoute t segment (RequesterT t request response m) where
  routeContext = lift routeContext
  withSegments f (RequesterT a) = RequesterT $ StrictState.mapStateT (mapReaderT $ withSegments f) a

instance Wrapped (RouteT t segment m a) where
  type Unwrapped (RouteT t segment m a) = ReaderT (RouteContext segment t) m a
  _Wrapped' = iso coerce coerce

instance RouteT t segment m a ~ x => Rewrapped (RouteT t segment m a) x

instance PerformEvent t m => PerformEvent t (RouteT t segment m) where
 type Performable (RouteT t segment m) = Performable m
 {-# INLINABLE performEvent_ #-}
 performEvent_ = lift . performEvent_
 {-# INLINABLE performEvent #-}
 performEvent = lift . performEvent

instance MonadRef m => MonadRef (RouteT t segment m) where
  type Ref (RouteT t segment m) = Ref m
  {-# INLINABLE newRef #-}
  newRef = lift . newRef
  {-# INLINABLE readRef #-}
  readRef = lift . readRef
  {-# INLINABLE writeRef #-}
  writeRef r = lift . writeRef r

instance (Adjustable t m, MonadHold t m) => Adjustable t (RouteT t segment m) where
  runWithReplace a0 a' = RouteT $ runWithReplace (unRouteT a0) (fmapCheap unRouteT a')
  traverseDMapWithKeyWithAdjust f dm edm = RouteT $ traverseDMapWithKeyWithAdjust (\k v -> unRouteT $ f k v) (coerce dm) (coerceEvent edm)
  {-# INLINABLE traverseDMapWithKeyWithAdjust #-}
  traverseDMapWithKeyWithAdjustWithMove f dm edm = RouteT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> unRouteT $ f k v) (coerce dm) (coerceEvent edm)

instance (DomBuilder t m, MonadHold t m, MonadFix m) => DomBuilder t (RouteT t segment m) where
  type DomBuilderSpace (RouteT t segment m) = DomBuilderSpace m
  textNode = lift . textNode
  element elementTag cfg (RouteT child) = RouteT $ element elementTag cfg child
  inputElement = lift . inputElement
  textAreaElement = lift . textAreaElement
  selectElement cfg (RouteT child) = RouteT $ selectElement cfg child
  placeRawElement = lift . placeRawElement
  wrapRawElement e = lift . wrapRawElement e

instance MonadReader r m => MonadReader r (RouteT t segment m) where
  ask = lift ask
  local f (RouteT a) = RouteT $ mapReaderT (local f) a

deriving instance MonadState s m => MonadState s (RouteT t segment m)

instance HasRoute t segment m => HasRoute t segment (EventWriterT t w m) where
  routeContext = lift routeContext
  withSegments f (EventWriterT a) = EventWriterT $ StrictState.mapStateT (withSegments f) a

instance EventWriter t w m => EventWriter t w (RouteT t segment m) where
  tellEvent = lift . tellEvent

instance HasRoute t segment m => HasRoute t segment (DynamicWriterT t w m) where
  routeContext = lift routeContext
  withSegments f (DynamicWriterT a) = DynamicWriterT $ StrictState.mapStateT (withSegments f) a

instance MonadDynamicWriter t w m => MonadDynamicWriter t w (RouteT t segment m) where
  tellDyn = lift . tellDyn

instance HasDocument m => HasDocument (RouteT t segment m)
instance HasJSContext m => HasJSContext (RouteT t segment m) where
  type JSContextPhantom (RouteT t segment m) = JSContextPhantom m
  askJSContext = RouteT askJSContext
#ifndef ghcjs_HOST_OS
instance MonadJSM m => MonadJSM (RouteT t segment m)
#endif


instance PrimMonad m => PrimMonad (RouteT t segment m) where
  type PrimState (RouteT t segment m) = PrimState m
  primitive = lift . primitive

--instance HasMountStatus t m => HasMountStatus t (RouteT segment r m) where
--  getMountStatus = lift getMountStatus

-- | Runs a monadic action (in 'RouteT') over the current URL as "interpreted"
-- by the given URL-parser. The action must return an 'Event' which can trigger
-- the route to change.
runRoute :: forall segment t m. (MonadWidget t m, Eq segment)
         => (forall a. URIRef a -> [segment])
         -> (forall a. URIRef a -> [segment] -> URIRef a)
         -> RouteT t segment m (Event t [segment])
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
                          , _routeContext_currentDepth   = 0
                          }
      newSegments <- runReaderT f ctx
      let x = ffor uri $ \uri' -> fromSegments uri' <$> newSegments
      pure (switch (current x))

-- | A very simple version of 'runRoute' that parses only the URL fragment
-- and splits it over @/@. Each path segment is therefore nothing more than 'Text'.
runRouteWithPathInFragment
  :: forall t m. (MonadWidget t m)
  => RouteT t Text m (Event t [Text])
  -> m ()
runRouteWithPathInFragment = runRoute
  (nullTextToEmptyList . T.splitOn "/" . T.dropAround (=='/') . fragAsText)
  (\oldUrl -> setFrag oldUrl . T.intercalate "/")
  where
    nullTextToEmptyList [""] = []
    nullTextToEmptyList x    = x

-- | Introduces a new "layer" in the nested routing tree. The given function takes
-- the current layer's route segment and builds the DOM for that segment.
withRoute
  :: forall a segment t m. (DomBuilder t m, MonadFix m, PostBuild t m, MonadHold t m, HasRoute t segment m, Eq segment)
  => (Maybe segment -> m a)
  -- ^ A routing function that produces a widget from a segment.
  -> m (Event t a)
withRoute f = do
  ctx <- routeContext
  let segmentsFlat = List.uncons <$> _routeContext_nextSegments ctx
  segmentsNested <- maybeDyn segmentsFlat

  let
    nextDepth = 1 + _routeContext_currentDepth ctx

    component = ffor segmentsNested $ \x -> case x of
      Nothing            -> do
        let
          segment = Nothing
          newCtx  = ctx{ _routeContext_currentSegment = pure segment
                       , _routeContext_nextSegments   = pure []
                       , _routeContext_currentDepth   = nextDepth
                       }

        a <- withSegments (const newCtx) (f segment)
        postBuildEv <- getPostBuild
        pure (a <$ postBuildEv)  -- Wrap the value up in an Event to unify both paths.

      Just segmentUncons -> do
        segmentDyn <- fmap Just <$> holdUniqDyn (fst <$> segmentUncons)
        let newCtx = ctx{ _routeContext_currentSegment = segmentDyn
                        , _routeContext_nextSegments   = snd <$> segmentUncons
                        , _routeContext_currentDepth   = nextDepth
                        }

        dyn $ ffor segmentDyn $ \segment ->
          withSegments (const newCtx) (f segment)

  switchPromptly never =<< dyn component

-- | All routing segments in the current URL.
allRouteSegments :: (MonadHold t m, MonadFix m, HasRoute t segment m, Eq segment) => m (Dynamic t [segment])
allRouteSegments = _routeContext_allSegments <$> routeContext

-- | The routing segment at "this layer" in the tree. "This layer" is defined by how many
-- nested 'withRoute's exist above the caller of this function.
currentRouteSegment :: (Functor m, HasRoute t segment m) => m (Dynamic t (Maybe segment))
currentRouteSegment = _routeContext_currentSegment <$> routeContext

-- | The next layer's segment in the tree. Like 'withRoute' this can be used to switch
-- over route segments, but it does not place a new layer in the tree.
nextRouteSegment :: (MonadHold t m, MonadFix m, HasRoute t segment m, Eq segment) => m (Dynamic t (Maybe segment))
nextRouteSegment = do
  ctx <- routeContext
  holdUniqDyn (listToMaybe <$> _routeContext_nextSegments ctx)

-- | Route segments from parent layers.
parentRouteSegments :: (MonadHold t m, MonadFix m, HasRoute t segment m, Eq segment) => m (Dynamic t [segment])
parentRouteSegments = do
  ctx <- routeContext
  holdUniqDyn $ do
    allSegments <- _routeContext_allSegments ctx
    pure $ take (_routeContext_currentDepth ctx - 1) allSegments

-- | A simple helper that produces a routing event on 'getPostBuild'.
redirectLocally :: (PostBuild t m) => [segment] -> m (Event t [segment])
redirectLocally segments = (segments <$) <$> getPostBuild


setFrag :: URIRef a -> Text -> URIRef a
setFrag uri p = uri & fragmentL .~ (Just $ encodeUtf8 p)

fragAsText :: URIRef a -> Text
fragAsText uri = maybe "" decodeUtf8 (uri ^. fragmentL)

pathSegments :: URIRef a -> [ByteString]
pathSegments uri =  uri ^. pathL . to (B8.split '/')
