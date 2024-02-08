(lang dune 3.12)
(using directory-targets 0.1)

(name ortografe)
(generate_opam_files true)

(package
 (name ortografe)
 (allow_empty)
 (depends ocaml dune
          ;; jj files | grep dune | xargs cat | sexp query 'each (variant libraries) each'  | cut -d . -f 1 | sort -u | grep -E -v '^(libraries|cmdliner_bindops|dict_gen|ortografe)$' | xargs echo
          bheap cmdliner core core_unix csv dream eio eio_main hyper markup patience_diff ppxlib re uri uucp uunf uuseg uutf zipc

 ))
