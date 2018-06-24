## Module FRP.Event.Time

#### `interval`

``` purescript
interval :: Int -> Event Instant
```

Create an event which fires every specified number of milliseconds.

#### `withTime`

``` purescript
withTime :: forall a. Event a -> Event { value :: a, time :: Instant }
```

Create an event which reports the current time in milliseconds since the epoch.

#### `debounce`

``` purescript
debounce :: forall a. Milliseconds -> Event a -> Event a
```

On each event, ignore subsequent events for a given number of milliseconds.

#### `debounceWith`

``` purescript
debounceWith :: forall a b. (Event a -> Event { period :: Milliseconds, value :: b }) -> Event a -> Event b
```

Provided an input event and transformation, block the input event for the
duration of the specified period on each output.


