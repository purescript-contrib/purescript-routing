# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes (😱!!!):

New features:

Bugfixes:

Other improvements:

## [v9.0.1](https://github.com/purescript-contrib/purescript-routing/releases/tag/v9.0.1) - 2020-02-09

- Bug fix: `param` will now remove `Query` from the internal state once all parameters have been consumed, allowing `end` to be used to disallow further parameters.

## [v9.0.0](https://github.com/purescript-contrib/purescript-routing/releases/tag/v9.0.0) - 2019-03-12

- Updated dependencies

## [v8.0.0](https://github.com/purescript-contrib/purescript-routing/releases/tag/v8.0.0) - 2018-05-28

- Updates for 0.12 and `Effect`.
- Removed `Match` class.

## [v7.1.0](https://github.com/purescript-contrib/purescript-routing/releases/tag/v7.1.0) - 2018-03-22

- Added matcher for `NonEmptyString`
- Re-exported `Routing.Match.Class` matchers from `Routing.Match`
- Added `Routing.PushState` module for using the browser's `pushState` interface instead of hash routing.

## [v7.0.0](https://github.com/purescript-contrib/purescript-routing/releases/tag/v7.0.0) - 2018-01-28

- Removes FFI
- Removes Aff functions
- Adds general `foldHashes`
- Removes `hashChanged`
- Changes the way `matches` works (filters failures)

@natefaubion

## [v6.1.2](https://github.com/purescript-contrib/purescript-routing/releases/tag/v6.1.2) - 2017-10-18

- Fix for parsing of URI-encoded path segments without query fragments

## [v6.1.1](https://github.com/purescript-contrib/purescript-routing/releases/tag/v6.1.1) - 2017-10-10

- Fixed behaviour of query parsing so that everything past the first `?` is parsed as a query string

## [v6.1.0](https://github.com/purescript-contrib/purescript-routing/releases/tag/v6.1.0) - 2017-10-03

- Added `Eq` and `Ord` instances for `RoutePart` (@coot)

## [v6.0.0](https://github.com/purescript-contrib/purescript-routing/releases/tag/v6.0.0) - 2017-09-23

- Updated for `purescript-aff` v4.x
- Altered parsing to capture query params without a trailing `/` #37
- Added `optionalMatch`

All updates courtesy of @coot

## [v5.1.0](https://github.com/purescript-contrib/purescript-routing/releases/tag/v5.1.0) - 2017-04-21

- Added `end` combinator @throughnothing

## [v5.0.0](https://github.com/purescript-contrib/purescript-routing/releases/tag/v5.0.0) - 2017-04-10

- Updates for PureScript 0.11 (@coot)

## [v4.0.0](https://github.com/purescript-contrib/purescript-routing/releases/tag/v4.0.0) - 2017-02-22

- Renamed `matchHash` and `matchHash'` to `match` and `matchWith`

## [v3.1.0](https://github.com/purescript-contrib/purescript-routing/releases/tag/v3.1.0) - 2017-02-18

- Added support for matching integers in paths (@menelaos)

## [v3.0.0](https://github.com/purescript-contrib/purescript-routing/releases/tag/v3.0.0) - 2016-11-03

- Updated for PureScript 0.10

## [v2.0.0](https://github.com/purescript-contrib/purescript-routing/releases/tag/v2.0.0) - 2016-07-31

- Updated dependencies

## [v1.0.0](https://github.com/purescript-contrib/purescript-routing/releases/tag/v1.0.0) - 2016-07-04

Updated for PureScript 0.9 and core libraries 1.0.

## [v0.4.0](https://github.com/purescript-contrib/purescript-routing/releases/tag/v0.4.0) - 2016-03-13

- Updated to latest `purescript-aff`

## [v0.2.1](https://github.com/purescript-contrib/purescript-routing/releases/tag/v0.2.1) - 2015-12-16

## [v0.2.0](https://github.com/purescript-contrib/purescript-routing/releases/tag/v0.2.0) - 2015-09-23

## [v0.1.0](https://github.com/purescript-contrib/purescript-routing/releases/tag/v0.1.0) - 2015-07-10
