`ppx_uchar_literal` provides a syntax for literals of type `Uchar.t`
(in expressions, not in patterns).

So `!!"é"` is equivalent to `Stdlib.Uchar.of_int 233`, but of course the latter
is hard to read.

There is an [open issue](https://github.com/ocaml/ocaml/issues/11999) to support
uchar literals directly in the language, which would be better than this.
