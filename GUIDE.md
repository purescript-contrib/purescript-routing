# A Guide to `purescript-routing`

`purescript-routing` consists of two core features

* An `Applicative` parsing framework for paths (`Routing.Match`)
* Routing effects and events (`Routing.Hash` or `Routing.PushState`)

## Parsing routes with `Routing.Match`

In many routing frameworks, you might start by using a stringly-typed DSL for
paths:

```js
const router = new Router();
router.on('/posts', () => {
  // Handle index
});
router.on('/posts/:postId', (postId) => {
  // Handle post
});
router.on('/posts/:postId/edit', (postId) => {
  // Handle edit
});
route.on('/posts/browse/:year/:month', (year, month) => {
  // Handle browsing
});
```

In this example interface, syntax of the form `:slug` indicates that the
value should be extracted from the path and provided to the callback handler.

With `purescript-routing`, we start by defining a _data type_ for our routes.

```purescript
type PostId = Int

data MyRoute
  = PostIndex
  | Post PostId
  | PostEdit PostId
  | PostBrowse String String
```

By using a data type, we can use `case` analysis to guarantee that we've
handled all possible routes both when dispatching and when constructing URLs.
If you can only construct a URL from your route, then it's impossible to
construct an invalid URL.

To turn a stringy path into our data type, we need to define a parser using
combinators in `Routing.Match` as well as standard `Applicative` and
`Alternative` combinators.

```purescript
import Prelude
import Control.Alternative ((<|>))
import Routing.Match (Match, lit, int, str, end)
```

The available `Match` combinators are:

* `lit` – Matches literal path segments. For example, `lit "posts"` would match
  the path segment "posts" in our example URL.
* `num` – Matches and returns a `Number` value.
* `int` – Matches and returns an `Int` value.
* `bool` – Matches and returns `true` or `false`.
* `str` – Returns the path segment as is.
* `param` – Extracts and returns a query parameter given a key.
* `params` – Returns all query parameters.
* `end` – Matches the end of the path.

Lets define a route for `PostIndex`. This route has no parameters, so all we
need to do is match the literal path segment "posts".

```purescript
postIndex :: Match Unit
postIndex = lit "posts"
```

However, this just yields a `Unit` value, and we need `MyRoute`. If there are
no interesting values we want to consume, we can use `<$` from `Prelude`.

```purescript
postIndex :: Match MyRoute
postIndex =
  PostIndex <$ lit "posts
```

Our next routes require extracting an integer `PostId`.

```purescript
post :: Match MyRoute
post =
  Post <$> lit "posts" *> int

postEdit :: Match MyRoute
postEdit =
  PostEdit <$> lit "posts" *> int <* lit "edit"
```

Note the use of the `*>` and `<*` operators. These let us direct the focus of
the value we want to consume. In `postEdit`, we want to consume the `int`,
but we also need to match the "edit" suffix. The arrows point to the value we
want.

And now finally, we need to extract multiple segments for `PostBrowse`.

```purescript
postBrowse :: Match MyRoute
postBrowse =
  PostBrowse <$> lit "posts" *> str <*> str
```

The `<*>` combinator has arrows on both sides because we want both values.
This works for any number of arguments our route needs. Just keep using
`<*>`.

Now to pull these all together, we can use `<|>` from `Control.Alternative`.
The routes will be tried in order until one matches.

```purescript
myRoute :: Match MyRoute
myRoute =
  postIndex <|> post <|> postEdit <|> postBrowse
```

Additionally, we can use `oneOf` from `Data.Foldable` which folds a data
structure using `<|>`.

```purescript
import Data.Foldable (oneOf)

myRoute :: Match MyRoute
myRoute = oneOf
  [ postIndex
  , post
  , postEdit
  , postBrowse
  ]
```

We can also go ahead and inline our parsers.

```purescript
myRoute :: Match MyRoute
myRoute = oneOf
  [ PostIndex <$ lit "posts"
  , Post <$> lit "posts" *> int
  , PostEdit <$> lit "posts" *> int <* lit "edit"
  , PostBrowse <$> lit "posts" *> str <*> str
  ]
```

You'll see we have some duplication. We are repeating the "posts" literal.
One of the great things about PureScript combinators is we can intuitively
factor things like this out and we know it will keep working. Since they all
start with "posts", we can just match that first.

```purescript
myRoute :: Match MyRoute
myRoute =
  lit "posts" *> oneOf
    [ pure PostIndex
    , Post <$> int
    , PostEdit <$> int <* lit "edit"
    , PostBrowse <$> str <*> str
    ]
```

This is a lot clearer, but we may have found a bug! Our first route is `pure
PostIndex`. There are no other conditions to match so this route will always
succeed, and our subsequent routes won't match. One thing we could do is
rearrange our routes so that `PostIndex` is last, but that just means
`PostIndex` will match anything under "posts". What we really want to do is
match "posts" exactly with no extra path segments. For that we should use the
`end` combinator.

```purescript
myRoute :: Match MyRoute
myRoute =
  lit "posts" *> oneOf
    [ PostIndex <$ end
    , Post <$> int <* end
    , PostEdit <$> int <* lit "edit" <* end
    , PostBrowse <$> str <*> str <* end
    ]
```

It seems like we might be able to factor out the `end` like we did with `lit
"posts"`, but that will bring us right back to our bug. It would match any of
the routes followed by an `end`, so we would still have to rearrange them.

```purescript
myRoute :: Match MyRoute
myRoute =
  lit "posts" *> oneOf
    [ PostEdit <$> int <* lit "edit"
    , Post <$> int
    , PostBrowse <$> str <*> str
    , pure PostIndex
    ] <* end
```

We've reduced duplication, but this might be more brittle under refactorings
since the ordering is very specific.

One last detail is the leading slash. `purescript-routing` doesn't require a
leading slash since URL hashes might not contain them, but we can match this
with the `root` combinator.

```purescript
myRoute :: Match MyRoute
myRoute =
  root *> lit "posts" *> oneOf
    [ PostEdit <$> int <* lit "edit"
    , Post <$> int
    , PostBrowse <$> str <*> str
    , pure PostIndex
    ] <* end
```

We can now test out our parser using `match`.

```purescript
import Routing (match)
import MyRoute (myRoute)

matchMyRoute :: String -> Either String MyRoute
matchMyRoute = match myRoute

test1 = matchMyRoute "/posts"
test2 = matchMyRoute "/posts/12"
test3 = matchMyRoute "/posts/12/edit"
test4 = matchMyRoute "/psots/bad"
```

## Routing events with `Routing.Hash`

Now that we have a parser, we'll want to respond to events and fire a
callback like in our original example. `purescript-routing` supports
hash-based routing via `Routing.Hash`.

```purescript
import Routing.Hash (matches)
import MyRoute (myRoute)
```

The `matches` combinator takes a `Match` parser and an `Effect` callback,
providing the previously matched route (wrapped in `Maybe` since it may be
the initial route) and the currently matched route. You might use this
callback to push an input to an instance of a running application.

```purescript
main = do
  matches myRoute \_ newRoute -> case newRoute of
    PostIndex -> ...
    Post postId -> ...
    PostEdit postId -> ...
    PostBrowse year month -> ...
```

Note that `matches` will _ignore_ routes that don't parse successfully. To
explicitly handle "not found" routes, we can add a fallback route.

```purescript
maybeMyRoute :: Match (Maybe MyRoute)
maybeMyRoute = oneOf
  [ Just <$> myRoute
  , pure Nothing
  ]

main = do
  matches maybeMyRoute \_ newRoute -> case newRoute of
    Nothing -> ... -- Not found
    Just PostIndex -> ...
    Just (Post postId) -> ...
    Just (PostEdit postId) -> ...
    Just (PostBrowse year month) -> ...
```

Alternatively, we could explicitly add a `NotFound` constructor to `MyRoute`.

## Routing events with `Routing.PushState`

Routing with `Routing.PushState` is similar to hash-based routing except that
we must first create an interface. Browsers don't handle location events
directly, so the interface needs to do some bookkeeping of it's own for
handling subscriptions.

```purescript
import Routing.PushState (makeInterface, matches)
import MyRoute (myRoute)
```

```purescript
main = do
  nav <- makeInterface
  nav # matches myRoute \_ newRoute -> case newRoute of
    PostIndex -> ...
    Post postId -> ...
    PostEdit postId -> ...
    PostBrowse year month -> ...
```

Use the created interface to push new states and routes. States are always
`Foreign` because they are global and may come from anywhere. We cannot
provide a well-typed interface with any guarantees.

```purescript
import Data.Foreign (toForeign)

main = do
  nav <- makeInterface
  ...
  nav.pushState (toForeign {}) "/about"
```

One option is to use `purescript-simple-json` which provides easy codecs to
and from `Foreign` for JSON-like data.

```purescript
import Simple.JSON (read, write)

type MyState =
  { foo :: String
  , bar :: Int
  }

main = do
  nav <- makeInterface
  _   <- nav.listen listener
  nav.pushState (write { foo: "foo", bar: 42 }) "/about"

  where
  listener location = case read location.state of
    Right { foo, bar } -> ...
    Left errors -> ...
```
