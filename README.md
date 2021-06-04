# fab

## Goals

fab provides a family monadic build systems for use inside programs.

It is roughly inspired by [Build Systems à la Carte](https://doi.org/10.1017/S0956796820000088),
but heavily refactored with type classes due to the following goals:

- Fabrications of one type can depend on fabrications of other types.
- Every key type can specify what strategy to use for build invalidation.

The following goals are not yet implemented:

- Batching - provide a mechanism (like [Haxl](https://hackage.haskell.org/package/haxl))
  to collect many fabrication requests for the same key type and process them
  simultaneously. This is most useful in Haxl-like contexts, fetching from
  data sources, but there may be other situations when fabricating a bunch of
  something is cheaper as a group than individually.
- Multithreading. The current schedulers are very simplistic and cannot run
  multiple fabricators at the same time.
