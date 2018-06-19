## Module FRP.Event.Class

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

#### `mapAccum`

``` purescript
mapAccum :: forall event a b c. IsEvent event => (a -> b -> Tuple b c) -> event a -> b -> event c
```

Map over an event with an accumulator.

For example, to keep the index of the current event:

```purescript
mapAccum (\x i -> Tuple (i + 1) (Tuple x i)) 0`.
```

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

#### `gate`

``` purescript
gate :: forall a event. IsEvent event => event Boolean -> event a -> event a
```

Sample the events that are fired while a boolean event is true. Note that,
until the boolean event fires, it will be assumed to be `false`, and events
will be blocked.

#### `gateBy`

``` purescript
gateBy :: forall a b event. IsEvent event => (Maybe a -> b -> Boolean) -> event a -> event b -> event b
```

Generalised form of `gateBy`, allowing for any predicate between the two
events. Until a value from the first event is received, `Nothing` will be
passed to the predicate.


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

