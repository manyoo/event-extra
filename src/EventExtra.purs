module FRP.Event.Extra where

import Prelude

import Data.Array (all, length, replicate, updateAt)
import Data.DateTime.Instant (Instant)
import Data.Filterable (filter)
import Data.Foldable (sequence_)
import Data.Int (fromNumber)
import Data.Maybe (Maybe(..), fromJust, fromMaybe, isJust)
import Data.Time.Duration (Milliseconds(..))
import Data.TraversableWithIndex (traverseWithIndex)
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Now (now)
import Effect.Ref as Ref
import Effect.Timer (clearTimeout, setTimeout)
import Effect.Unsafe (unsafePerformEffect)
import FRP.Event (Event, create, makeEvent, subscribe, withLast)
import Partial.Unsafe (unsafePartial)


-- | create an Event that will fire in n milliseconds
after :: Int -> Event Instant
after n = makeEvent \k -> do
    id <- setTimeout n do
        time <- now
        k time
    pure (clearTimeout id)

-- | delay the input event by n milliseconds
-- TODO: add code to clear timeouts when cancelled
delay :: forall a. Int -> Event a -> Event a
delay n evt = makeEvent \k -> do
    subscribe evt \v -> do
        setTimeout n (k v)

debounce :: forall a. Milliseconds -> Event a -> Event a
debounce (Milliseconds period) evt = makeEvent \k -> do
    timer <- Ref.new Nothing
    
    subscribe evt \v -> do
        tf <- Ref.read timer
        case tf of
            Just t -> clearTimeout t
            Nothing -> pure unit
        newT <- setTimeout (fromMaybe 1 $ fromNumber period) (k v)
        Ref.write (Just newT) timer

-- | skip first n occurrences of the event
skip :: forall a. Int -> Event a -> Event a
skip n evt = makeEvent \k -> do
    fired <- Ref.new 0
    subscribe evt \v -> do
        newCount <- Ref.modify ((+) 1) fired
        if newCount > n
        then k v
        else pure unit

distinct :: forall a. Eq a => Event a -> Event a
distinct evt = getNow <$> filter isDiff (withLast evt)
    where isDiff { last, now } = last == Just now
          getNow { now } = now


-- | this is the similar to the 'fold' function in FRP.Event.
-- But this function will send the default value as well.
foldWithDef :: forall a b. (a -> b -> b) -> Event a -> b -> Event b
foldWithDef f e b = makeEvent \k -> do
    result <- Ref.new b
    timer <- setTimeout 10 (k b)
    dispose <- subscribe e \a -> Ref.modify (f a) result >>= k
    pure (clearTimeout timer *> dispose)


-- | merge an array of events into an event of an array of the current values.
-- it will fire whenever one event in the array fires and all events have already
-- fired with valid values.
mergeArray :: forall a. Array (Event a) -> Event (Array a)
mergeArray arr = makeEvent \k -> do
    let initVals = replicate (length arr) Nothing
    values <- Ref.new initVals

    let newValFunc i val = do
            newVals <- Ref.modify (fromMaybe [] <<< updateAt i (Just val)) values
            if all isJust newVals
            then k (unsafePartial fromJust <$> newVals)
            else pure unit

    disposables <- traverseWithIndex (\i e -> subscribe e (newValFunc i)) arr
    
    pure $ sequence_ disposables

-- | Perform events with actions inside.
performEvent :: forall a. Event (Effect a) -> Event a
performEvent evt = multicast $ makeEvent \k -> subscribe evt \act -> act >>= k

-- | fold events with Effect actions 
foldEffect :: forall a b. (a -> b -> Effect b) -> Event a -> b -> Event b
foldEffect act e b = makeEvent \k -> do
    result <- Ref.new b
    subscribe e \a -> do
        curB <- Ref.read result
        newB <- act a curB
        Ref.write newB result
        k newB

multicast :: forall a. Event a -> Event a
multicast evt = unsafePerformEffect $ do
    { event: newEvt, push: p } <- create
    dispose <- subscribe evt p
    pure newEvt

debug :: forall a. Show a => Event a -> Event a
debug evt = performEvent $ f <$> evt
    where f v = logShow v *> pure v

debugWith :: forall a. (a -> Effect Unit) -> Event a -> Event a
debugWith f evt = performEvent $ g <$> evt
    where g v = f v *> pure v
