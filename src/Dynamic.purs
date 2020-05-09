module FRP.Dynamic where

import Prelude

import Control.Plus (empty)
import Data.Array (updateAt)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Traversable (sequence_, traverse)
import Data.TraversableWithIndex (traverseWithIndex)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Ref (Ref, new, read, write)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Event (Event, makeEvent, subscribe)
import FRP.Event as Event
import FRP.Event.Extra (debugWith, performEvent)

newtype Dynamic a = Dynamic {
    event   :: Event a,
    current :: Ref a
}

dynEvent :: forall a. Dynamic a -> Event a
dynEvent (Dynamic d) = d.event

current :: forall a. Dynamic a -> Effect a
current (Dynamic d) = read d.current

-- internal function to create a new event from old event and save all values
-- fired in the event into the Ref value
mkNewEvt :: forall a. Ref a -> Event a -> Event a
mkNewEvt ref evt = makeEvent \k -> subscribe evt \v -> k v *> write v ref

step :: forall a. a -> Event a -> Dynamic a
step def evt = Dynamic { event: mkNewEvt cur evt, current: cur }
    where cur = unsafePerformEffect $ new def

instance functorDynamic :: Functor Dynamic where
    map f d = step def (f <$> dynEvent d)
        where def = f $ unsafePerformEffect $ current d

instance applyDynamic :: Apply Dynamic where
    apply = mergeWith (\f v -> f v)

instance applicativeDynamic :: Applicative Dynamic where
    pure a = step a empty

instance semigroupDynamic :: Semigroup a => Semigroup (Dynamic a) where
    append = mergeWith append

instance monoidDynamic :: Monoid a => Monoid (Dynamic a) where
    mempty = step mempty mempty


subscribeDyn :: forall a r. Dynamic a -> (a -> Effect r) -> Effect (Effect Unit)
subscribeDyn d func = do
    v <- current d
    _ <- func v
    subscribe (dynEvent d) func

performDynamic :: forall a. Dynamic (Effect a) -> Dynamic a
performDynamic d = step def (performEvent $ dynEvent d)
    where def = unsafePerformEffect $ current d >>= identity

fold :: forall a b. (a -> b -> b) -> Dynamic a -> b -> Dynamic b
fold f d v = step def (Event.fold f (dynEvent d) def)
    where def = f (unsafePerformEffect $ current d) v

keepLatest :: forall a. Dynamic (Dynamic a) -> Dynamic a
keepLatest d = step def (Event.keepLatest (dynEvent <$> dynEvent d))
    where def = unsafePerformEffect do
                    v <- current =<< current d
                    pure v

mergeWith :: forall a b c. (a -> b -> c) -> Dynamic a -> Dynamic b -> Dynamic c
mergeWith f a b = Dynamic { event: evt, current: def }
    where def = unsafePerformEffect do
                    va <- current a
                    vb <- current b
                    new $ f va vb
          evt = makeEvent \k -> do
                    d1 <- subscribe (dynEvent a) \v -> do
                              vb <- current b
                              let c = f v vb
                              k c
                              write c def
                    d2 <- subscribe (dynEvent b) \v -> do
                              va <- current a
                              let c = f va v
                              k c
                              write c def
                    pure $ d1 *> d2

mergeDynArray :: forall a. Array (Dynamic a) -> Dynamic (Array a)
mergeDynArray arr = Dynamic { event: evt, current: def }
    where def = unsafePerformEffect $ traverse current arr >>= new
          evt = makeEvent \k -> do
                    let newValFunc i val = do
                            old <- read def
                            let new = fromMaybe old $ updateAt i val old
                            k new
                            write new def
                    ds <- traverseWithIndex (\i d -> subscribe (dynEvent d) (newValFunc i)) arr
                    pure $ sequence_ ds

withLast :: forall a. Dynamic a -> Dynamic { last :: Maybe a, now :: a }
withLast d = step def evt
    where def = unsafePerformEffect do
                    v <- current d
                    pure { last: Nothing, now: v }
          evt = makeEvent \k -> subscribe (dynEvent d) \v -> do
                    ov <- current d
                    pure { last: Just ov, now: v }

sampleDyn :: forall a b. Dynamic a -> Event (a -> b) -> Event b
sampleDyn d evt = makeEvent \k -> subscribe evt \f -> current d >>= f >>> k

sampleDyn_ :: forall a b. Dynamic a -> Event b -> Event a
sampleDyn_ d evt = sampleDyn d (f <$> evt)
    where f _ a = a

gateDyn :: forall a. Dynamic Boolean -> Event a -> Event a
gateDyn d e = makeEvent \k -> subscribe e \v -> do
                  gv <- current d
                  when gv (k v)

debugDyn :: forall a. Show a => Dynamic a -> Dynamic a
debugDyn = debugDynWith logShow

debugDynWith :: forall a. (a -> Effect Unit) -> Dynamic a -> Dynamic a
debugDynWith f d = step def (debugWith f $ dynEvent d)
    where def = unsafePerformEffect do
                    v <- current d
                    f v
                    pure v
