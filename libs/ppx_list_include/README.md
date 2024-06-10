% ppx\_list\_include

# What it does

`ppx_list_include` extends the syntax of list literals with a "splice" operator:
for instance, `[ 1; +l; 3 ]` is preprocessed into `List.concat [ [1]; l; [3] ]`.

# Why

This is just a minor syntactic shortcut. Sometimes, like building html (as trees) or
process arguments, list literals can feel rather limited because you frequently need to
splice lists into lists, and starting with a list and turning it into a `List.concat`
or similar later adds a bit of friction.

For instance, if you start with:

```ocaml
Process.run
    ~prog:"diff"
    ~args:[ "--"; file1; file2 ]
    ()
```

and then want to pass more options:

```ocaml
Process.run
    ~prog:"diff"
    ~args:(List.concat
            [ (match unified with
              | None -> []
              | Some ctx -> [ "-U=" ^ Int.to_string ctx ])
            ; if side_by_side then ["-y"] else []
            ; [ "--"
              ; file1
              ; file2
              ]
            ])
    ()
```

Instead, with this ppx, you can write:

```ocaml
Process.run
    ~prog:"diff"
    ~args:[ +(match unified with
              | None -> []
              | Some ctx -> [ "-U=" ^ Int.to_string ctx ])
          ; +if side_by_side then ["-y"] else []
          ; "--"
          ; file1
          ; file2
          ]
    ()
```
