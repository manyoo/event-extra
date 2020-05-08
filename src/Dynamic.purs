module FRP.Dynamic where

import Prelude

import Control.Plus (empty)
import Effect (Effect)
import Effect.Ref (Ref, new, read)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import FRP.Event (Event, subscribe)
import FRP.Event.Extra (performEvent)

newtype Dynamic a = Dynamic {
    event   :: Event a,
    current :: Ref a
}

instance functorDynamic :: Functor Dynamic where
    map f (Dynamic { event, current }) = Dynamic {
        event   : f <$> event,
        current : unsafePerformEffect do
                    v <- read current
                    new $ f v
    }

instance applyDynamic :: Apply Dynamic where
    apply (Dynamic f) (Dynamic d) = Dynamic {
        event   : apply f.event d.event,
        current : unsafePerformEffect do
                    func <- read f.current
                    v <- read d.current
                    new $ func v
    }

instance applicativeDynamic :: Applicative Dynamic where
    pure a = Dynamic { event: empty, current: unsafePerformEffect (new a) }

instance semigroupDynamic :: Semigroup a => Semigroup (Dynamic a) where
    append (Dynamic d1) (Dynamic d2) = Dynamic {
        event   : append d1.event d2.event,
        current : unsafePerformEffect do
                    v1 <- read d1.current
                    v2 <- read d2.current
                    new $ append v1 v2
    }

instance monoidDynamic :: Monoid a  => Monoid (Dynamic a) where
    mempty = Dynamic { event: mempty, current: unsafePerformEffect (new mempty) }


step :: forall a. a -> Event a -> Dynamic a
step def evt = Dynamic { event: evt, current: unsafePerformEffect (Ref.new def) }

dynEvent :: forall a. Dynamic a -> Event a
dynEvent (Dynamic { event }) = event

dynCurrent :: forall a. Dynamic a -> Effect a
dynCurrent (Dynamic { current }) = read current

performDynamic :: forall a. Dynamic (Effect a) -> Dynamic a
performDynamic (Dynamic { event, current }) = Dynamic {
    event   : performEvent event,
    current : unsafePerformEffect do
                v <- read current
                res <- v
                new res
}

subscribeDyn :: forall a r. Dynamic a -> (a -> Effect r) -> Effect (Effect Unit)
subscribeDyn d f = do
    cur <- dynCurrent d
    _ <- f cur
    subscribe (dynEvent d) f
