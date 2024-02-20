(lang dune 3.12)
(using directory-targets 0.1)

(name ortografe)
(generate_opam_files true)

(package
 (name ortografe)
 (allow_empty)
 (depends ocaml dune
          base bheap cmdliner core core_unix dream eio eio_main hyper js_of_ocaml lwt_ppx markup patience_diff ppx_compare ppx_jane ppxlib ppx_sexp_conv ppx_sexp_value ppx_string re sexplib uri uucp uunf uuseg uutf yojson zipc ;; MARKER

 ))
