# trivial-utf-16

This tiny Common Lisp library includes translators between UTF-32 and
UTF-16. Its purpose is to be able to gloss over the differences between
implementations that use UTF-16 internally (notably, ABCL and Allegro) and those
that use UTF-32 internally (for instance, CCL and SBCL). This enables you to
implement Unicode algorithms in terms of code points, even if characters are
confined to 16 bits.

The specific algorithm this was implemented for is normalisation,
[UAX-15](https://www.unicode.org/reports/tr15/), but it’s likely to be useful
for anything that needs to deal with the so-called “astral planes” of Unicode –
such as the emoji. 😱

## Scope

The library exports six symbols:

- `encode-utf-16` has a function that takes a vector of Unicode code points and
  turns anything higher than U+FFFF into a pair of surrogate code
  points. Otherwise it ignores surrogates.
- `decode-utf-16` has a function that takes a vector of UTF-16 code units and
  turns matched pairs of surrogates into code points above U+FFFF. It passes
  unpaired surrogates through.
- `to-unicode-string` has a function that tries to take a Lisp string and turn
  it into a vector of code points. It will pass unpaired surrogates if the Lisp
  implementation has them.
- `from-unicode-string` has a function that tries to translate a vector of code
  points into a Lisp string.
- `unicode-point` has a type definition that allows it to hold Unicode code
  points.
- `unicode-string` has a type definition of a vector of Unicode code points.

The two functions that deal with Lisp strings use `char-code-limit` as a
heuristic, encoding to and from UTF-16 if that value indicates that character
codes are 16 bits.

As the descriptions above show, this library is liberal on the question of
surrogate code points. This is because many known implementations of UTF-16 are
similarly liberal. This is strictly speaking a violation of the Unicode
standards, and some Common Lisp implementations may enforce the standard.

## Status

The code is essentially non-optimised at this point. I have no doubt whatever
that there are better ways of doing this. If you know of any, I would be glad to
learn of them.

## Licencing

> Do what thou wilt.

This code, and its accompanying documentation, is distributed under the
[CC0](https://creativecommons.org/publicdomain/zero/1.0/) public domain
dedication.
