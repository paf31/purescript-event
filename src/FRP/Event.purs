module FRP.Event
  ( Event
  , EventIO
  , create
  , makeEvent
  , subscribe
  , module Class
  ) where

import Prelude

import Control.Alternative (class Alt, class Alternative, class Plus)
import Control.Apply (lift2)
import Data.Array (deleteBy)
import Data.Either (either, fromLeft, fromRight, hush, isLeft, isRight)
import Data.Compactable (class Compactable)
import Data.Filterable (class Filterable, filterMap)
import Data.Foldable (sequence_, traverse_)
import Data.Maybe (Maybe(..), fromJust, isJust)
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Unsafe (unsafePerformEffect)
import FRP.Event.Class (class Filterable, class IsEvent, count, filterMap, fix,
                        fold, folded, gate, gateBy, keepLatest, mapAccum,
                        sampleOn, sampleOn_, withLast) as Class
import Partial.Unsafe (unsafePartial)
import Unsafe.Reference (unsafeRefEq)

-- | An `Event` represents a collection of discrete occurrences with associated
-- | times. Conceptually, an `Event` is a (possibly-infinite) list of values-and-times:
-- |
-- | ```purescript
-- | type Event a = List { value :: a, time :: Time }
-- | ```
-- |
-- | Events are created from real events like timers or mouse clicks, and then
-- | combined using the various functions and instances provided in this module.
-- |
-- | Events are consumed by providing a callback using the `subscribe` function.
newtype Event a = Event ((a -> Effect Unit) -> Effect (Effect Unit))

instance functorEvent :: Functor Event where
  map f (Event e) = Event \k -> e (k <<< f)

instance compactableEvent :: Compactable Event where
  compact xs = map (\x -> unsafePartial fromJust x) (filter isJust xs)
  separate xs =
    { left: unsafePartial (map fromLeft) (filter isLeft xs)
    , right: unsafePartial (map fromRight) (filter isRight xs)
    }

instance filterableEvent :: Filterable Event where
  filter = filter

  filterMap f = map (\x -> unsafePartial fromJust x) <<< filter isJust <<< map f

  partition p xs = { yes: filter p xs, no: filter (not <<< p) xs }

  partitionMap f xs =
    { left: filterMap (either Just (const Nothing) <<< f) xs
    , right: filterMap (hush <<< f) xs
    }

instance applyEvent :: Apply Event where
  apply (Event e1) (Event e2) = Event \k -> do
    latestA <- Ref.new Nothing
    latestB <- Ref.new Nothing
    c1 <- e1 \a -> do
      Ref.write (Just a) latestA
      Ref.read latestB >>= traverse_ (k <<< a)
    c2 <- e2 \b -> do
      Ref.write (Just b) latestB
      Ref.read latestA >>= traverse_ (k <<< (_ $ b))
    pure (c1 *> c2)

instance applicativeEvent :: Applicative Event where
  pure a = Event \k -> do
    k a
    pure (pure unit)

instance altEvent :: Alt Event where
  alt (Event f) (Event g) = Event \k -> do
    c1 <- f k
    c2 <- g k
    pure (c1 *> c2)

instance plusEvent :: Plus Event where
  empty = Event \_ -> pure (pure unit)

instance alternativeEvent :: Alternative Event

instance semigroupEvent :: Semigroup a => Semigroup (Event a) where
  append = lift2 append

instance monoidEvent :: Monoid a => Monoid (Event a) where
  mempty = pure mempty

instance eventIsEvent :: Class.IsEvent Event where
  fold = fold
  keepLatest = keepLatest
  sampleOn = sampleOn
  fix = fix

-- | Fold over values received from some `Event`, creating a new `Event`.
fold :: forall a b. (a -> b -> b) -> Event a -> b -> Event b
fold f (Event e) b = Event \k -> do
  result <- Ref.new b
  e \a -> Ref.modify (f a) result >>= k

-- | Create an `Event` which only fires when a predicate holds.
filter :: forall a. (a -> Boolean) -> Event a -> Event a
filter p (Event e) = Event \k -> e \a -> if p a then k a else pure unit

-- | Create an `Event` which samples the latest values from the first event
-- | at the times when the second event fires.
sampleOn :: forall a b. Event a -> Event (a -> b) -> Event b
sampleOn (Event e1) (Event e2) = Event \k -> do
  latest <- Ref.new Nothing
  c1 <- e1 \a -> do
    Ref.write (Just a) latest
  c2 <- e2 \f -> do
    Ref.read latest >>= traverse_ (k <<< f)
  pure (c1 *> c2)

-- | Flatten a nested `Event`, reporting values only from the most recent
-- | inner `Event`.
keepLatest :: forall a. Event (Event a) -> Event a
keepLatest (Event e) = Event \k -> do
  cancelInner <- Ref.new Nothing
  cancelOuter <- e \inner -> do
    Ref.read cancelInner >>= sequence_
    c <- subscribe inner k
    Ref.write (Just c) cancelInner
  pure do
    Ref.read cancelInner >>= sequence_
    cancelOuter

-- | Compute a fixed point
fix :: forall i o. (Event i -> { input :: Event i, output :: Event o }) -> Event o
fix f = Event \k -> do
    c1 <- subscribe input push
    c2 <- subscribe output k
    pure (c1 *> c2)
  where
    { event, push } = unsafePerformEffect create
    { input, output } = f event

-- | Subscribe to an `Event` by providing a callback.
-- |
-- | `subscribe` returns a canceller function.
subscribe
  :: forall r a
   . Event a
  -> (a -> Effect r)
  -> Effect (Effect Unit)
subscribe (Event e) k = e (void <<< k)

-- | Make an `Event` from a function which accepts a callback and returns an
-- | unsubscription function.
-- |
-- | Note: you probably want to use `create` instead, unless you need explicit
-- | control over unsubscription.
makeEvent
  :: forall a
   . ((a -> Effect Unit) -> Effect (Effect Unit))
  -> Event a
makeEvent = Event

type EventIO a =
  { event :: Event a
  , push :: a -> Effect Unit
  }

-- | Create an event and a function which supplies a value to that event.
create
  :: forall a
   . Effect (EventIO a)
create = do
  subscribers <- Ref.new []
  pure
    { event: Event \k -> do
        _ <- Ref.modify (_ <> [k]) subscribers
        pure do
          _ <- Ref.modify (deleteBy unsafeRefEq k) subscribers
          pure unit
    , push: \a -> do
        Ref.read subscribers >>= traverse_ \k -> k a
    }
