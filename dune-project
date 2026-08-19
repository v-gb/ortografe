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
          (ocamlformat (and :with-dev-setup (= 0.29.0)))
          (sexp :with-dev-setup)
          (ocaml-lsp-server :with-dev-setup)
          base base64 bheap brr cmdliner core core_unix dream eio eio_main js_of_ocaml lwt_ppx patience_diff ppx_assert ppx_bin_prot ppx_compare ppx_inline_test ppx_jane ppx_pipebang ppx_sexp_conv ppx_sexp_value ppx_string ppxlib re sexplib uri uucp uunf uuseg uutf yojson zipc ;; MARKER

 ))
