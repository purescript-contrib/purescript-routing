# A Guide to `purescript-routing`

A couple notes upfront:

* This library facilitates hash-based routing.  If you're looking to do pushstate routing with the [history](https://developer.mozilla.org/en-US/docs/Web/API/History_API) object, then you are in the wrong place.
* Routes are declared using [applicative](https://pursuit.purescript.org/packages/purescript-prelude/0.1.4/docs/Prelude#t:Applicative) syntax.  If you're not yet comfortable with applicatives, see this [chapter](http://learnyouahaskell.com/functors-applicative-functors-and-monoids#applicative-functors) or this [paper](http://www.staff.city.ac.uk/~ross/papers/Applicative.pdf).

#### Usage

First, define some locations:

```purescript

module SolarSystemRoutes where

import Prelude
import Control.Alt ((<|>))
import Control.Apply
import Data.Functor
import Data.Map
import Routing
import Routing.Match
import Routing.Match.Class
import Routing.Hash
import Data.Int (floor)

data Locations
  = Home
  | Jupiter     -- The place where Boys go to get 'stupider'.
  | Mars        -- Girls seek fame here.
  | MPC Int     -- Minor Planet Circular (poor Pluto, et al.)
  | Moon String -- They're not all made of cheese.
  | PlanetWithAttributes (Map String String) -- Looking for a new home?

```
We'll use `Locations` instead of `Routes` to avoid confusion with the `Route` (singular) type exposed by the library. But let's discuss the types later.  For now, we'll see it in action:

```purescript
oneSlash :: Match Unit
oneSlash = lit "/"

homeSlash :: Match Unit
homeSlash = lit ""

int :: Match Int
int = floor <$> num

home :: Match Locations
home = Home <$ oneSlash

jupiter :: Match Locations
jupiter = Jupiter <$ (homeSlash *> lit "jupiter")

mars :: Match Locations
mars = Mars <$ (homeSlash *> lit "mars" *> lit "hollywood")

mpc :: Match Locations
mpc = MPC <$> (homeSlash *> lit "mpc" *> int)

moon :: Match Locations
moon = Moon <$> (homeSlash *> lit "moon" *> str)

planetWithAttributes :: Match Locations
planetWithAttributes = PlanetWithAttributes <$> (homeSlash *> lit "planet" *> params)

```

So what's all this?  `purescript-routing` provides a `Match` type that is an instance of the `MatchClass`.  The `MatchClass` encompasses types that provide the route-matching primitives:
* `lit`: A function that takes a string, and returns a `Match` that can be used to match the provided string.
* `str`: A `Match` that captures a string.
* `num`: A `Match` that captures a javascript number.
* `param`: A function that takes a parameter key (i.e., `String`), and returns a `Match` that will pluck the value associated with the key from the query parameter block.
* `params`: A `Match` that captures the query parameters.
* `bool`:  A `Match` for `true` or `false`.


(Note that `lit ""` -- aliased above as `homeSlash` -- matches a single url forward-slash.)

`Match` is functor.  So we can map over it.  Above, for example, we map the `floor` function over `num :: Match Number` and we get `int :: Match Int`.  We map our `Locations` constructors over other `Match`s to yield a `Match Locations`.

`Match` is a newtype that looks like this:

```purescript
newtype Match a = Match (Route -> V (Free MatchError) (Tuple Route a))
```

Don't worry too much about the scary type for now.  Essentially, `Match` wraps a function that takes a `Route` (represented as a `List RoutePart`, where `RouteParts` is either the stuff between url slashes or the query parameters) and returns a `V` functor.  `V` stands for 'validation' and you can think of it as an `Either` that can return more than one error upon failure.  So, in essence, something of type `Match Locations` is a function from `Route -> Either Errors (Tuple (Rest-of-RoutePart-List) Locations)`.

The real magic happens in `Match`'s `Apply (i.e., <*>)` implementation.

The `Apply` instance for `Match` provides the plumbing that allows you to compose your routes.  The `<*>`, `*>` and `<*` operators are your friends.  Like any other applicative, `(leftMatch *> rightMatch)` performs the left `Match`, discards a successful result, and returns the value of the right `Match`.  `(leftMatch <* rightMatch)` does the converse.  So, `MPC <$> (homeSlash *> lit "mpc" *> int)` maps the `MPC` constructor function over a `Match` that will parse a slash (and discard it), parse the literal string "mpc" (discarding it too), and return the parsed integer.

Routes are combined using `Match`'s `Alt (<|>)` instance.

```purescript
routing :: Match Locations
routing =
  jupiter <|>
  mars    <|>
  mpc     <|>
  moon    <|>
  home
```
`jupiter (<|>) mars` means "try Jupiter, and if that fails, try Mars".  Like many routing DSLs, this means that more general routes come after more specific ones.  Thus, `home` (i.e., "/") comes last (unless you only ever want to go home).

We've got routes.  Now what do we do with them?

`purescript-routing` provides a couple functions that help you make use of your `Match`s.

```purescript
matches :: forall e a. Match a -> (Maybe a -> a -> Eff e Unit) -> Eff e Unit
```

and

```purescript
matchesAff :: forall e a. Match a -> Aff e (Tuple (Maybe a) a)
```

`matches` and `matchesAff` provide access to the stream of hash changes.  You provide a function with your `routing` (e.g., your composed `Match Locations`), and `matches` (or `matchesAff`) maps it over the stream.

In our example, we'll run in `Eff`:

```purescript
main = do
  matches routing (\old new -> someAction old new)
        --- other stuff ---
  where
    someAction :: forall e. Maybe Locations -> Locations -> Eff e Unit
    someAction = ...
```
The previous route (`old`) is a `Maybe Locations` and the `new` is of type `Locations`.  The `someAction` you take is up to you, but you'll probably want to do something with the `new` value.
