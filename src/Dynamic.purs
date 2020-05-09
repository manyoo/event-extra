module FRP.Dynamic where

import Prelude

import Control.Plus (empty)
import Effect (Effect)
import Effect.Ref (Ref, new, read, write)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Event (Event, makeEvent, subscribe)
import FRP.Event as Event
import FRP.Event.Extra (performEvent)

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
mkNewEvt ref evt = makeEvent \k -> subscribe evt \v -> write v ref *> k v

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
                              write c def
                              k c
                    d2 <- subscribe (dynEvent b) \v -> do
                              va <- current a
                              let c = f va v
                              write c def
                              k c
                    pure $ d1 *> d2

sampleDyn :: forall a b. Dynamic a -> Event b -> Event a
sampleDyn d evt = makeEvent \k -> subscribe evt \_ -> current d >>= k

gateDyn :: forall a. Dynamic Boolean -> Event a -> Event a
gateDyn d e = makeEvent \k -> subscribe e \v -> do
                  gv <- current d
                  when gv (k v)
