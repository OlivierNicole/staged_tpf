# Metaprogramming-based deriving

This aims to offer the same functionality as ppx_deriving, but based on
type-safe metaprogramming, in order to make new derivers infinitely easier to
write and trivial to maintain.

It is an adaptation of [tpf](https://github.com/pqwy/tpf/),
combined with the metaprogramming PPX
[ppx_stage](https://github.com/stedolan/ppx_stage) so that generic functions
have no interpretative overhead.

It is nowhere close to usable for now as it is very much WIP and the
documentation is yet to be written. However, the principles are the same as for
tpf, which is [well documented](https://pqwy.github.io/tpf/doc/tpf/index.html).

To do:

- Work around the value restriction which makes it annoying to instantiate
  generic functions
- Make the interface more usable
- Write documentation
- Write derivers for the most common needs (`iter`, `show`, `eq`, `ord`...)
