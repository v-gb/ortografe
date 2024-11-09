(lang dune 3.12)
(using directory-targets 0.1)

(formatting
 (enabled_for ocaml))

(name ortografe)
(generate_opam_files true)

(package
 (name ortografe)
 (allow_empty)
 (depends ocaml dune
          (ocamlformat (and :with-dev-setup (= 0.26.2)))
          base base64 bheap brr cmdliner core core_unix dream eio eio_main hyper js_of_ocaml lwt_ppx patience_diff ppx_assert ppx_bin_prot ppx_compare ppx_inline_test ppx_jane ppxlib ppx_pipebang ppx_sexp_conv ppx_sexp_value ppx_string re sexplib stdio uri uucp uunf uuseg uutf yojson zipc ;; MARKER

 ))
