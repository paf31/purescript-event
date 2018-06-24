## Module FRP.Event

#### `fix`

``` purescript
fix :: forall i o. (Event i -> { input :: Event i, output :: Event o }) -> Event o
```

Compute a fixed point

#### `fold`

``` purescript
fold :: forall a b. (a -> b -> b) -> Event a -> b -> Event b
```

Fold over values received from some `Event`, creating a new `Event`.

#### `keepLatest`

``` purescript
keepLatest :: forall a. Event (Event a) -> Event a
```

Flatten a nested `Event`, reporting values only from the most recent
inner `Event`.

#### `sampleOn`

``` purescript
sampleOn :: forall a b. Event a -> Event (a -> b) -> Event b
```

Create an `Event` which samples the latest values from the first event
at the times when the second event fires.

#### `Event`

``` purescript
newtype Event a
```

An `Event` represents a collection of discrete occurrences with associated
times. Conceptually, an `Event` is a (possibly-infinite) list of values-and-times:

```purescript
type Event a = List { value :: a, time :: Time }
```

Events are created from real events like timers or mouse clicks, and then
combined using the various functions and instances provided in this module.

Events are consumed by providing a callback using the `subscribe` function.

##### Instances
``` purescript
Functor Event
Compactable Event
Filterable Event
Apply Event
Applicative Event
Alt Event
Plus Event
Alternative Event
(Semigroup a) => Semigroup (Event a)
(Monoid a) => Monoid (Event a)
IsEvent Event
```

#### `create`

``` purescript
create :: forall a. Effect { event :: Event a, push :: a -> Effect Unit }
```

Create an event and a function which supplies a value to that event.

#### `makeEvent`

``` purescript
makeEvent :: forall a. ((a -> Effect Unit) -> Effect (Effect Unit)) -> Event a
```

Make an `Event` from a function which accepts a callback and returns an
unsubscription function.

Note: you probably want to use `create` instead, unless you need explicit
control over unsubscription.

#### `subscribe`

``` purescript
subscribe :: forall r a. Event a -> (a -> Effect r) -> Effect (Effect Unit)
```

Subscribe to an `Event` by providing a callback.

`subscribe` returns a canceller function.


### Re-exported from Data.Filterable:

#### `Filterable`

``` purescript
class (Compactable f, Functor f) <= Filterable f  where
  filterMap :: forall a b. (a -> Maybe b) -> f a -> f b
```

`Filterable` represents data structures which can be _partitioned_/_filtered_.

- `partitionMap` - partition a data structure based on an either predicate.
- `partition` - partition a data structure based on boolean predicate.
- `filterMap` - map over a data structure and filter based on a maybe.
- `filter` - filter a data structure based on a boolean.

Laws:
- Functor Relation: `filterMap identity ≡ compact`
- Functor Identity: `filterMap Just ≡ identity`
- Kleisli Composition: `filterMap (l <=< r) ≡ filterMap l <<< filterMap r`

- `filter ≡ filterMap <<< maybeBool`
- `filterMap p ≡ filter (isJust <<< p)`

- Functor Relation: `partitionMap identity ≡ separate`
- Functor Identity 1: `_.right <<< partitionMap Right ≡ identity`
- Functor Identity 2: `_.left <<< partitionMap Left ≡ identity`

- `f <<< partition ≡ partitionMap <<< eitherBool` where `f = \{ no, yes } -> { left: no, right: yes }`
- `f <<< partitionMap p ≡ partition (isRight <<< p)` where `f = \{ left, right } -> { no: left, yes: right}`

Default implementations are provided by the following functions:

- `partitionDefault`
- `partitionDefaultFilter`
- `partitionDefaultFilterMap`
- `partitionMapDefault`
- `filterDefault`
- `filterDefaultPartition`
- `filterDefaultPartitionMap`
- `filterMapDefault`

##### Instances
``` purescript
Filterable Array
Filterable Maybe
(Monoid m) => Filterable (Either m)
Filterable List
(Ord k) => Filterable (Map k)
```

### Re-exported from FRP.Event.Class:

#### `IsEvent`

``` purescript
class (Alternative event, Filterable event) <= IsEvent event  where
  fold :: forall a b. (a -> b -> b) -> event a -> b -> event b
  keepLatest :: forall a. event (event a) -> event a
  sampleOn :: forall a b. event a -> event (a -> b) -> event b
  fix :: forall i o. (event i -> { input :: event i, output :: event o }) -> event o
```

Functions which an `Event` type should implement:

- `fold`: combines incoming values using the specified function,
starting with the specific initial value.
- `keepLatest` flattens a nested event, reporting values only from the
most recent inner event.
- `sampleOn`: samples an event at the times when a second event fires.
- `fix`: compute a fixed point, by feeding output events back in as
inputs.

#### `withLast`

``` purescript
withLast :: forall event a. IsEvent event => event a -> event { now :: a, last :: Maybe a }
```

Compute differences between successive event values.

#### `sampleOn_`

``` purescript
sampleOn_ :: forall event a b. IsEvent event => event a -> event b -> event a
```

Create an `Event` which samples the latest values from the first event
at the times when the second event fires, ignoring the values produced by
the second event.

#### `mapAccum`

``` purescript
mapAccum :: forall event a b c. IsEvent event => (a -> b -> Tuple b c) -> event a -> b -> event c
```

Map over an event with an accumulator.

For example, to keep the index of the current event:

```purescript
mapAccum (\x i -> Tuple (i + 1) (Tuple x i)) 0`.
```

#### `gateBy`

``` purescript
gateBy :: forall a b event. IsEvent event => (Maybe a -> b -> Boolean) -> event a -> event b -> event b
```

Generalised form of `gateBy`, allowing for any predicate between the two
events. Until a value from the first event is received, `Nothing` will be
passed to the predicate.

#### `gate`

``` purescript
gate :: forall a event. IsEvent event => event Boolean -> event a -> event a
```

Sample the events that are fired while a boolean event is true. Note that,
until the boolean event fires, it will be assumed to be `false`, and events
will be blocked.

#### `folded`

``` purescript
folded :: forall event a. IsEvent event => Monoid a => event a -> event a
```

Combine subsequent events using a `Monoid`.

#### `count`

``` purescript
count :: forall event a. IsEvent event => event a -> event Int
```

Count the number of events received.

