% ppx\_lazy\_option\_op

`ppx_lazy_option_op` is a ppx that provides a `||?` operator of type
`'a option -> 'a -> 'a`, i.e like `Option.value`, but lazy.

The operator name is meant to be reminiscent of `||`, so the laziness is easy
to get used to.

Examples :
```ocaml
value ||? default()

input ||? failwith "--in-place only makes sense with an INPUT_FILE"
```
