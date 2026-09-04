# json-pointer

JSON Pointers ([RFC 6901](https://datatracker.ietf.org/doc/html/rfc6901)) for Aeson.

This library implements:

- A `JsonPointer` type, which can be constructed from fragments via a `Monoid` instance
- A parser and a printer, including support for the `~1` and `~0` escape sequences
- `FromJSON`, `ToJSON` and `ToSchema` (openapi3) instances
- Functions for pointing into Aeson `Value`s

Parser accepts both the plain form ("/foo/bar") and the relative URI form ("#/foo/bar"), but no URL decoding is performed in either case. 

Printer always produces the plain form.

## Example

You can use `make repl` to start a REPL with the library loaded.

```haskell
>>> import Data.Aeson
>>> import Data.JsonPointer

>>> let doc = object ["foo" .= [object ["bar" .= (1 :: Int)]]]
>>> let pointer = atKey "foo" <> atIndex 0 <> atKey "bar"

>>> pointTo pointer doc
Just (Number 1.0)

>>> show pointer
"/foo/0/bar"

>>> parseJsonPointer "#/foo/0/bar"
Right /foo/0/bar
```

## Checks

```
make check         # format-check, lint and test
make format        # apply the formatting instead of just checking it
```

The individual checks are `make format-check` (fourmolu over `src` and `test`, cabal-fmt
over the cabal file), `make lint` (hlint) and `make test`. `make help` lists the rest.

The tools are not part of the build, install them once:

```
cabal install fourmolu cabal-fmt hlint
```

The same three checks run on every pull request, see `.github/workflows/ci.yaml`.

## Provenance

This library is based off of https://hackage.haskell.org/package/json-pointer.

The license to the original code is included in the LICENSE file.
