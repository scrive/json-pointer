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

```haskell
>>> import Data.Aeson
>>> import Data.JsonPointer

>>> let doc = object ["foo" .= [object ["bar" .= (1 :: Int)]]]
>>> let pointer = atKey "foo" <> atIndex 0 <> atKey "bar"

>>> value pointer doc
Just (Number 1.0)

>>> show pointer
"/foo/0/bar"

>>> parseJsonPointer "#/foo/0/bar"
Right /foo/0/bar
```


## Provenance

This library is based off of https://hackage.haskell.org/package/json-pointer.

The license to the original code is included in the LICENSE file.
